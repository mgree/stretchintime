module Expr exposing (..)

{- TODO

 - semantics tests
 - parser (and tests)
 - pretty printer (and tests)

-}

import Dict exposing (Dict)
import Random
import Random.List

import Common exposing (..)
import Plan exposing (..)

type alias ActionInfo =
    { name : String
    , duration : Seconds
    }

type alias IntersperseInfo =
    { sep : Expr
    , expr : Expr
    , before : Bool
    , after : Bool
    }

type Expr = Action ActionInfo
          | Pause Seconds
          | Message String
          | Vary Key (List String) Expr
          | Repeat Int Expr
          | Seq (List Expr)
          | Shuffle Expr
          | Intersperse IntersperseInfo

sampleExpr = 
    Intersperse
    { sep = Seq [Pause 7, Message "3 seconds...", Pause 3]
    , expr = Seq [ Action { name =  "forward fold", duration = 60 }
                  , Vary "side" ["right", "left"]
                      (Action { name = "quad", duration = 60 })
                  , Repeat 3
                      (Vary "side" ["right", "left"]
                           (Action { name = "glute", duration = 60 }))
                  ]
    , before = True
    , after = False
    }

sampleExpr2 = Shuffle (Seq [Message "one", Message "two", Message "three", Message "four"])

type ExprError = ActionEmptyName ActionInfo
               | ActionInvalidTime ActionInfo
               | PauseInvalidTime Seconds
               | MessageEmpty
               | VaryEmpty Key Expr
               | VaryEmptyName (List String) Expr
               | RepeatInvalidCount Int Expr
               | ShuffleSingleton Expr
               | EmptySeq

countExpr : Expr -> Int
countExpr exprOuter =
    case exprOuter of
        Action _ -> 1

        Pause _ -> 1

        Message _ -> 1

        Vary _ options expr ->
            List.length options * countExpr expr

        Repeat count expr ->
            count * countExpr expr

        Seq exprs ->
            List.sum (List.map countExpr exprs)

        Shuffle expr ->
            countExpr expr

        Intersperse info ->
            let sepCount = countExpr info.sep
                exprCount = countExpr info.expr

                seps = exprCount - 1 + 
                       (if info.before then 1 else 0) +
                       (if info.after then 1 else 0)
            in

                seps * sepCount + exprCount

check : Expr -> List ExprError
check exprOuter = 
    case exprOuter of
        Action info -> 
            (if String.isEmpty info.name
             then [ActionEmptyName info]
             else [])
            ++
            (if info.duration <= 0
             then [ActionInvalidTime info]
             else [])

        Pause seconds ->
            (if seconds <= 0
             then [PauseInvalidTime seconds]
             else [])

        Message msg -> 
            (if String.isEmpty msg
             then [MessageEmpty]
             else [])

        Vary key options expr ->
            (if String.isEmpty key
             then [VaryEmptyName options expr]
             else [])
            ++
            (if List.isEmpty options
             then [VaryEmpty key expr]
             else [])
            ++
            check expr

        Repeat count expr ->
            (if count <= 0
             then [RepeatInvalidCount count expr]
             else [])
            ++
            check expr

        Seq [] ->
            [EmptySeq]

        Seq exprs -> 
            List.concatMap check exprs

        Shuffle expr -> 
            (if countExpr expr == 1
             then [ShuffleSingleton expr]
             else [])
            ++
            check expr

        Intersperse info ->
            check info.sep ++ check info.expr


toPlan : Expr -> Random.Seed -> (Plan, Random.Seed)
toPlan exprOuter seed0 =
    case exprOuter of
        Action info -> 
            ([ Plan.Action (infoToPlanActionInfo info) ], seed0)
                
        Pause seconds -> 
            ([ Plan.Pause seconds ], seed0)

        Message msg ->
            ([ Plan.Announce msg ], seed0)

        Vary key optionList expr -> 
            let (entries, seed1) = toPlan expr seed0

                options = case optionList of
                              [] -> [key]
                              _ -> optionList
            in

            ( List.concatMap 
                  (\option -> List.map (addKeyToEntry key option) entries)
                  options
            , seed1)

        Repeat count expr ->
            let (entries, seed1) = toPlan expr seed0 in

            ( List.concat (List.repeat count entries)
            , seed0)

        Seq exprs -> 
            List.foldr
                (\expr (allEntries,seed1) ->
                     let (entries, seed2) = toPlan expr seed1 in
                     (entries ++ allEntries, seed2))
                ([], seed0)
                exprs

        Shuffle expr ->
            let (entries, seed1) = toPlan expr seed0 in
            
            Random.step (Random.List.shuffle entries) seed1

        Intersperse info ->
            let (sepPlan, seed1) = toPlan info.sep seed0
                (exprPlan, seed2) = toPlan info.expr seed1

                -- intersperse, but with a list sep
                combine : List a -> List a -> List a
                combine sep l =
                    case l of
                        [] -> []
                        [e] -> [e]
                        e::es -> [e] ++ sep ++ combine sep es
            in          
                ( (if info.before then sepPlan else []) ++
                  combine sepPlan exprPlan ++
                  (if info.after then sepPlan else [])
                , seed2)
            
infoToPlanActionInfo : ActionInfo -> Plan.ActionInfo
infoToPlanActionInfo info = 
    { name = info.name
    , meta = Dict.empty
    , duration = info.duration
    }


