module DateExtra exposing (..)

import Date exposing (Date)
import String


unsafeFromString : String -> Date
unsafeFromString string =
    case Date.fromString string of
        Ok date ->
            date

        Err msg ->
            Debug.crash <| "unsafeFromString: " ++ string


fromStringWithFallback : String -> Date -> Date
fromStringWithFallback string fallback =
    case Date.fromString string of
        Ok date ->
            date

        Err _ ->
            fallback


toISOString : Date -> String
toISOString date =
    let
        year =
            Date.year date

        month =
            Date.month date

        day =
            Date.day date
    in
        String.join "-"
            [ String.padLeft 4 '0' <| toString year
            , String.padLeft 2 '0' <| toString (monthToInt month)
            , String.padLeft 2 '0' <| toString day
            ]


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12
