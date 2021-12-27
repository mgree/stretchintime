module Common exposing (..)

type alias Seconds = Int
type alias Millis = Int

type alias Key = String

formatHMS : Seconds -> String
formatHMS totalSeconds =
    let (hours, minutes, seconds) = secondsToHMS totalSeconds in
    let strs = [ if hours > 0 then String.fromInt hours ++ ":" else ""
               , if hours > 0
                 then twoDigitInt minutes
                 else if minutes > 0
                      then String.fromInt minutes
                      else ""
               , if hours > 0 || minutes > 0
                 then twoDigitInt seconds
                 else String.fromInt seconds
               ]
    in
    List.filter (not << String.isEmpty) strs |> String.join ":"

secondsToHMS : Seconds -> (Int, Int, Seconds)
secondsToHMS totalSeconds =              
    let seconds = modBy 60 totalSeconds in
    let totalMinutes = totalSeconds // 60 in
    let minutes = modBy 60 totalMinutes in
    let hours = totalMinutes // 60 in
    (hours, minutes, seconds)

twoDigitInt : Int -> String
twoDigitInt n = String.padLeft 2 '0' (String.fromInt n)
