module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time

main = Browser.element
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

type Msg = Tick Time.Posix
         | InitializeTime Time.Zone Time.Posix

type alias State = ()

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
    { state : State
    , time : Time.Posix
    , config : Config
    }

withTime : Time.Posix -> Model -> Model
withTime newTime model = { model | time = newTime }

withZone : Time.Zone -> Model -> Model
withZone newZone model = { model | config = model.config |> configWithZone newZone }

configWithZone : Time.Zone -> Config -> Config
configWithZone newZone config = { config | zone = newZone }

init : () -> (Model, Cmd Msg)
init () = ( { state = ()
            , time = Time.millisToPosix 0
            , config = defaultConfig
            }
          , Task.perform (\x -> x) (Task.map2 InitializeTime Time.here Time.now)
          )

view : Model -> Html Msg
view model = 
    h1 [ class "clock" ]
       [ text (clockTime model.config model.time) ]

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


