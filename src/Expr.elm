module Expr exposing (..)

{- TODO

 - semantics (and tests)
 - parser (and tests)
 - pretty printer (and tests)

-}

import Dict exposing (Dict)
import Random
import Random.List

type alias Key = String

type alias Seconds = Int

type alias EntryInfo =
    { name : String
    , duration : Seconds
    }

type alias IntersperseInfo =
    { sep : Expr
    , expr : Expr
    , before : Bool
    , after : Bool
    }

type Expr = Entry EntryInfo
          | Pause Seconds
          | Message String
          | Vary Key (List String) Expr
          | Repeat Int Expr
          | Group Key Expr
          | Seq (List Expr)
          | Shuffle Expr
          | Intersperse IntersperseInfo

type alias Plan = List PlanEntry

type alias ActionInfo =
    { name : String
    , meta : Dict Key (List String)
    , duration : Seconds
    }


type PlanEntry = Action ActionInfo
               | Gap Seconds
               | Announce String

type ExprError = EntryEmptyName EntryInfo
               | EntryInvalidTime EntryInfo
               | PauseInvalidTime Seconds
               | MessageEmpty
               | VaryEmpty Key Expr
               | VaryEmptyName (List String) Expr
               | VaryGroup (List String) Expr
               | RepeatInvalidCount Int Expr
               | GroupEmptyName Expr
               | ShuffleSingleton Expr
               | EmptySeq

countExpr : Expr -> Int
countExpr exprOuter =
    case exprOuter of
        Entry _ -> 1

        Pause _ -> 1

        Message _ -> 1

        Vary _ options expr ->
            List.length options * countExpr expr

        Repeat count expr ->
            count * countExpr expr

        Group _ expr ->
            countExpr expr

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
        Entry info -> 
            (if String.isEmpty info.name
             then [EntryEmptyName info]
             else [])
            ++
            (if info.duration <= 0
             then [EntryInvalidTime info]
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
            (if key == "group"
             then [VaryGroup options expr]
             else [])
            ++
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

        Group group expr ->
            (if String.isEmpty group
             then [GroupEmptyName expr]
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
        Entry info -> 
            ([ Action (entryToAction info) ], seed0)
                
        Pause seconds -> 
            ([ Gap seconds ], seed0)

        Message msg ->
            ([ Announce msg ], seed0)

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

        Group group expr -> 
            let (entries, seed1) = toPlan expr seed0 in

            ( List.map (addKeyToEntry "group" group) entries
            , seed1)

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
            
entryToAction : EntryInfo -> ActionInfo
entryToAction info = { name = info.name
                     , meta = Dict.empty
                     , duration = info.duration
                     }

addKeyToEntry : Key -> String -> PlanEntry -> PlanEntry
addKeyToEntry key val entry =
    case entry of
        Action info -> 
            Action (addKeyToInfo key val info)
                       
        Gap _ -> entry

        Announce _ -> entry

addKeyToInfo : Key -> String -> ActionInfo -> ActionInfo
addKeyToInfo key val info =
    { info | 
          meta = Dict.update key 
                 (\mOldVal -> 
                      case mOldVal of
                          Nothing -> Just [val]
                          Just oldVal -> Just (val::oldVal))
                 info.meta
    }
