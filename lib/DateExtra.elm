module DateExtra exposing (..)

import Date exposing (Date)
import Time


fromStringWithFallback : String -> Date -> Date
fromStringWithFallback string fallback =
    Date.fromIsoString string |> Result.withDefault fallback


{-| Milliseconds since the Unix epoch at midnight UTC on the given date
-}
toTime : Date -> Float
toTime date =
    toFloat (Date.toRataDie date - Date.toRataDie epoch) * 86400 * 1000


epoch : Date
epoch =
    Date.fromCalendarDate 1970 Time.Jan 1
