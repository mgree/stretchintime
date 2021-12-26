module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
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

--------------------------------------------------------------------------------
-- MSG and CONFIG and MODEL
--------------------------------------------------------------------------------

type Msg = Tick Time.Posix
         | InitializeTime Time.Zone Time.Posix
         | NewSeed Random.Seed

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
    { expr : Expr
    , completed : Plan
    , current : Maybe PlanEntry
    , pending : Plan
    , time : Time.Posix
    , seed : Random.Seed
    , config : Config
    }

withTime : Time.Posix -> Model -> Model
withTime newTime model = { model | time = newTime }

withZone : Time.Zone -> Model -> Model
withZone newZone model = { model | config = model.config |> configWithZone newZone }

configWithZone : Time.Zone -> Config -> Config
configWithZone newZone config = { config | zone = newZone }

withSeed : Random.Seed -> Model -> Model
withSeed newSeed model = { model | seed = newSeed }

resetPlan : Model -> Model
resetPlan model = 
    let (plan, seed1) = toPlan model.expr model.seed in
    { model | completed = []
            , current = Nothing
            , pending = plan
            , seed = seed1    
    }

--------------------------------------------------------------------------------
-- INIT
--------------------------------------------------------------------------------

init : () -> (Model, Cmd Msg)
init () = ( { expr = Expr.sampleExpr2
            , completed = []
            , current = Nothing
            , pending = []
            , time = Time.millisToPosix 0
            , config = defaultConfig
            , seed = Random.initialSeed 0
            }
          , Cmd.batch [ Task.perform (\x -> x) (Task.map2 InitializeTime Time.here Time.now)
                      , Random.generate NewSeed Random.independentSeed
                      ]
          )

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------

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
                  
        NewSeed newSeed -> ( model |> withSeed newSeed |> resetPlan
                           , Cmd.none)

--------------------------------------------------------------------------------
-- SUBSCRIPTIONS
--------------------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch 
        [ Time.every 250 Tick 
        ]

--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------

view : Model -> Html a
view model = 
    div [ ]
        [ h1 [ class "clock" ]
             [ text (clockTime model.config model.time) ]
        , div [ class "current" ]
              (case model.current of
                  Nothing -> []
                  Just entry -> [ h2 [] [text "current"]
                                , viewEntry entry])
        , div [ class "pending" ]
              [ h2 [] [text "pending"]
              , ol [] (model.pending |> List.map (\entry -> ul [] [viewEntry entry]))
              ]
        , div [ class "completed" ]
              [ h2 [] [text "completed"]
              , ol [] (model.completed |> List.map (\entry -> ul [] [viewEntry entry]))
              ]
        ] 

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

timeDifference : Time.Posix -> Time.Posix -> Seconds
timeDifference timeNow timeThen =
    let nowFloored  = Time.posixToMillis timeNow // 1000
        thenFloored = (Time.posixToMillis timeThen // 1000)
    in abs (nowFloored - thenFloored)
