module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Task
import Time

import Common exposing (..)
import Expr exposing (Expr)
import Plan exposing (Plan)

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

type alias CurrentEntry =
    { entry : Plan.Entry
    , duration : Seconds
    , elapsed : Millis
    , last : Time.Posix
    }

type alias Model = 
    { expr : Expr
    , completed : Plan
    , current : Maybe CurrentEntry
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
    let (plan, seed1) = Expr.toPlan model.expr model.seed in
    { model | completed = []
            , current = Nothing
            , pending = plan
            , seed = seed1    
    }

--------------------------------------------------------------------------------
-- INIT
--------------------------------------------------------------------------------

init : () -> (Model, Cmd Msg)
init () = ( { expr = Expr.sampleExpr
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

        Tick newTime -> model |> withTime newTime |> advancePlan 
                  
        NewSeed newSeed -> model |> withSeed newSeed |> resetPlan |> advancePlan

advancePlan : Model -> (Model, Cmd Msg)
advancePlan model =
    case model.current of
        Nothing -> 
            case model.pending of
                [] -> -- all done
                    (model, Cmd.none)
                (entry::entries) -> -- pick off the next pending entry
                    ( { model | pending = entries
                              , current = Just { entry = entry
                                               , duration = Plan.duration entry
                                               , elapsed = 0
                                               , last = model.time
                                               }
                      }
                    , Cmd.none)
        Just info0 -> 
            let
                -- computation in milliseconds

                now : Millis
                now = Time.posixToMillis model.time

                last : Millis
                last = Time.posixToMillis info0.last

                sinceLast : Millis
                sinceLast = now - last

                info1 = { info0 | elapsed = info0.elapsed + sinceLast 
                                , last = model.time
                        }

                -- convert to seconds to see if we're done
                elapsed : Seconds
                elapsed = info1.elapsed // 1000

            in

                if elapsed >= info1.duration
                then
                    { model | current = Nothing 
                            , completed = model.completed ++ [info1.entry]
                    } |> advancePlan
                else
                    ( { model | current = Just info1 }
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
        [ div [ class "current" ]
              (case model.current of
                  Nothing -> []
                  Just info -> [ h2 [] [text "current"]
                                , viewEntry info.entry
                                , viewRemaining info
                               ])
        , div [ class "pending" ]
              [ h2 [] [text "pending"]
              , ol [] (model.pending |> List.map (\entry -> ul [] [viewEntry entry]))
              ]
        , div [ class "completed" ]
              [ h2 [] [text "completed"]
              , ol [] (model.completed |> List.map (\entry -> ul [] [viewEntry entry]))
              ]
        , pre [ class "debug" ]
              [ text "-- CURRENT\n"
              , (case model.current of
                     Nothing -> ""
                     Just info -> info.entry |> Plan.entryToString) |> text
              , text "\n-- PENDING\n"
              , model.pending |> Plan.toString |> text
              , text "\n-- COMPLETED\n"
              , model.completed |> Plan.toString |> text
              ]
        ] 

viewRemaining : CurrentEntry -> Html a
viewRemaining info =
    let 
        elapsed : Seconds
        elapsed = info.elapsed // 1000

        remaining : Seconds
        remaining = info.duration - elapsed
    in
        
    span [ class "remaining" ]
         [ remaining |> formatHMS |> text ]

viewEntry : Plan.Entry -> Html a
viewEntry entry =
    case entry of
        Plan.Action info ->
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

        Plan.Pause seconds ->
            div [ class "entry", class "pause" ]
                [ text "rest "
                , span [ class "duration" ] [ seconds |> String.fromInt |> text ] 
                ]

        Plan.Announce msg ->
            div [ class "entry", class "announce" ]
                [ text msg ]


