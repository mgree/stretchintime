module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time

import Expr exposing (..)

{- TODO

   crib checkTimers for core logic for running task
   keep index of the current one

-}

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

type Msg = Tick Time.Posix
         | InitializeTime Time.Zone Time.Posix

type alias Config = 
    { zone : Time.Zone
    , twelveHour : Bool
    }

defaultConfig : Config
defaultConfig = 
    { zone = Time.utc
    , twelveHour = True
    }

type alias Model = 
    { state : Expr
    , time : Time.Posix
    , config : Config
    }

withTime : Time.Posix -> Model -> Model
withTime newTime model = { model | time = newTime }

withZone : Time.Zone -> Model -> Model
withZone newZone model = { model | config = model.config |> configWithZone newZone }

configWithZone : Time.Zone -> Config -> Config
configWithZone newZone config = { config | zone = newZone }

sampleExpr = 
    Intersperse
    { sep = Seq [Pause 7, Message "3 seconds...", Pause 3]
    , expr = Group "standing" 
             (Seq [ Entry { name =  "forward fold", duration = 60 }
                  , Vary "side" ["right", "left"]
                      (Entry { name = "quad", duration = 60 })
                  , Repeat 3
                      (Vary "side" ["right", "left"]
                           (Entry { name = "glute", duration = 60 }))
                  ]
             )
    , before = True
    , after = False
    }

init : () -> (Model, Cmd Msg)
init () = ( { state = sampleExpr
            , time = Time.millisToPosix 0
            , config = defaultConfig
            }
          , Task.perform (\x -> x) (Task.map2 InitializeTime Time.here Time.now)
          )

view : Model -> Html a
view model = 
    div [ ]
        ([ h1 [ class "clock" ]
              [ text (clockTime model.config model.time) ]        
         ] ++ (model.state |> toPlan |> List.map viewEntry))

viewEntry : PlanEntry -> Html a
viewEntry entry =
    case entry of
        Action info ->
            div [ class "entry", class "action" ]
                [ span [ class "name" ] [ text info.name ]
                , span [ class "duration" ] [ info.duration |> String.fromInt |> text ]
                , div [ class "meta" ]
                    (   info.meta
                     |> Dict.toList
                     |> List.map 
                            (\(k,vs) ->
                                 div [ class "kv" ]
                                   [ span [ class "key" ]
                                          [ text k ]
                                   , ol [ class "values" ]
                                       (List.map (\v -> ul [] [ text v]) vs)
                                   ]
                            )
                    )
                ]

        Gap seconds ->
            div [ class "entry", class "gap" ]
                [ text "rest "
                , span [ class "duration" ] [ seconds |> String.fromInt |> text ] 
                ]

        Announce msg ->
            div [ class "entry", class "announce" ]
                [ text msg ]


clockTime : Config -> Time.Posix -> String
clockTime config time = 
  let baseHour  = Time.toHour config.zone time
      hour      = formatHour config baseHour
      minute    = twoDigitInt    (Time.toMinute config.zone time)
      second    = twoDigitInt    (Time.toSecond config.zone time)
      ampm      = if config.twelveHour
                  then if baseHour >= 12
                       then "pm"
                       else "am"
                  else ""
  in
  hour ++ ":" ++ minute ++ ":" ++ second ++ ampm

formatHour : Config -> Int -> String
formatHour config baseHour =
    let hourNum = if config.twelveHour
                  then modBy 12 baseHour
                  else baseHour
        hour12  = if hourNum == 0 then 12 else hourNum
    in
        String.fromInt hour12

twoDigitInt : Int -> String
twoDigitInt n = String.padLeft 2 '0' (String.fromInt n)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitializeTime newZone newTime ->
            ( model 
            |> withTime newTime 
            |> withZone newZone
            , Cmd.none)

        Tick newTime -> ( model |> withTime newTime -- TODO check timers
                        , Cmd.none)
                  
subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch 
        [ Time.every 250 Tick 
        ]

timeDifference : Time.Posix -> Time.Posix -> Seconds
timeDifference timeNow timeThen =
    let nowFloored  = Time.posixToMillis timeNow // 1000
        thenFloored = (Time.posixToMillis timeThen // 1000)
    in abs (nowFloored - thenFloored)
