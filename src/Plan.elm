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
           | Gap Seconds
           | Announce String

addKeyToEntry : Key -> String -> Entry -> Entry
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

duration : Entry -> Seconds
duration entry =
    case entry of
        Action info -> info.duration

        Gap seconds -> seconds

        Announce _ -> 0

--------------------------------------------------------------------------------
-- PLAN PRETTY PRINTING
--------------------------------------------------------------------------------


