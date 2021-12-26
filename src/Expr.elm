module Expr exposing (..)

{- TODO

 - semantics (and tests)
 - parser (and tests)
 - pretty printer (and tests)

-}

import Dict exposing (Dict)

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
          | Message String -- TODO
          | Vary Key (List String) Expr
          | Repeat Int Expr -- TODO
          | Group Key Expr
          | Seq (List Expr)
          | Intersperse IntersperseInfo
            -- shuffle (needs generators w/ step :/)

type alias Plan = List PlanEntry

type alias ActionInfo =
    { name : String
    , meta : Dict Key (List String)
    , duration : Seconds
    }


type PlanEntry = Action ActionInfo
               | Gap Seconds
               | Announce String -- TODO

type ExprError = VaryEmpty Key Expr
               | VaryEmptyName (List String) Expr
               | VaryGroup (List String) Expr
               | GroupEmptyName Expr
               | EntryEmptyName EntryInfo
               | EntryInvalidTime EntryInfo
               | PauseInvalidTime Seconds
               | EmptySeq

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

        Intersperse info ->
            check info.sep ++ check info.expr

toPlan : Expr -> Plan
toPlan exprOuter =
    case exprOuter of
        Entry info -> 
            [ Action (entryToAction info) ]
                
        Pause seconds -> 
            [ Gap seconds ]

        Vary key optionList expr -> 
            let entries = toPlan expr

                options = case optionList of
                              [] -> [key]
                              _ -> optionList
            in

            List.concatMap 
                (\option -> List.map (addKeyToEntry key option) entries)
                options

        Group group expr -> 
            List.map (addKeyToEntry "group" group) (toPlan expr)

        Seq exprs -> 
            List.concatMap toPlan exprs

        Intersperse info ->
            let sepPlan = toPlan info.sep
                exprPlan = toPlan info.expr 

                -- intersperse, but with a list sep
                combine : List a -> List a -> List a
                combine sep l =
                    case l of
                        [] -> []
                        [e] -> [e]
                        e::es -> [e] ++ sep ++ combine sep es
            in          
                (if info.before then sepPlan else []) ++
                combine sepPlan exprPlan ++
                (if info.after then sepPlan else [])
            
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
