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
        rounded =
            roundTo precision float

        intLength =
            truncate (logBase 10 rounded) + 1

        baseString =
            toString rounded

        string =
            if String.contains "." baseString then
                baseString
            else
                baseString ++ "."

        totalLength =
            intLength + 1 + precision
    in
        string |> String.padRight totalLength '0'
