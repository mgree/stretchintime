module Plan exposing (..)

import Dict exposing (Dict)

import Common exposing (..)

type alias Plan = List Entry

type alias ActionInfo =
    { name : String
    , meta : Dict Key (List String)
    , duration : Seconds
    }


type Entry = Action ActionInfo
           | Pause Seconds
           | Announce String

addKeyToEntry : Key -> String -> Entry -> Entry
addKeyToEntry key val entry =
    case entry of
        Action info -> 
            Action (addKeyToInfo key val info)
                       
        Pause _ -> entry

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

duration : Entry -> Seconds
duration entry =
    case entry of
        Action info -> info.duration

        Pause seconds -> seconds

        Announce _ -> 0

--------------------------------------------------------------------------------
-- PLAN PRETTY PRINTING
--------------------------------------------------------------------------------

{-
PLAN  ::= ENTRY 
        | ENTRY
          PLAN

ENTRY ::= NAME DURATION (NAME: VALUE)*
        | pause DURATION
        | announce MESSAGE
        
NAME     ::= [^0-9]*
VALUE    ::= [^,]*
DURATION ::= [[H:]M:]S
MESSAGE  ::= [^\n]*
-}

toString : Plan -> String
toString plan = plan |> List.map entryToString |> String.join "\n"

entryToString : Entry -> String
entryToString entry =
    case entry of

        Action info -> Debug.todo "action"

        Pause seconds -> Debug.todo "gap"

        Announce msg -> Debug.todo "announce"
