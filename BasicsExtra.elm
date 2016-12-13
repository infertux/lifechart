module BasicsExtra exposing (..)


roundTo : Int -> Float -> Float
roundTo precision float =
    let
        factor =
            toFloat (10 ^ precision)
    in
        (float * factor |> round |> toFloat) / factor


roundToPadded : Int -> Float -> String
roundToPadded precision float =
    let
        intLength =
            1 + logBase 10 float |> truncate

        string =
            roundTo precision float |> toString

        totalLength =
            if toFloat (truncate float) == float then
                intLength
            else
                intLength + 1 + precision
    in
        string |> String.padRight totalLength '0'
