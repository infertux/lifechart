module ColorExtra exposing (fromHex, toHex, mixHsl)

import Color exposing (Color)


hexDigits : String
hexDigits =
    "0123456789abcdef"


{-| Parse a "#rrggbb" string
-}
fromHex : String -> Maybe Color
fromHex string =
    let
        channel index =
            case String.toList (String.slice index (index + 2) (String.toLower string)) of
                [ high, low ] ->
                    Maybe.map2 (\h l -> h * 16 + l) (digit high) (digit low)

                _ ->
                    Nothing

        digit char =
            List.head (String.indexes (String.fromChar char) hexDigits)
    in
        if String.length string == 7 && String.startsWith "#" string then
            Maybe.map3 Color.rgb255 (channel 1) (channel 3) (channel 5)
        else
            Nothing


{-| Format a color as a "#rrggbb" string
-}
toHex : Color -> String
toHex color =
    let
        { red, green, blue } =
            Color.toRgba color

        digit int =
            String.slice int (int + 1) hexDigits

        channel value =
            let
                int =
                    round (value * 255)
            in
                digit (int // 16) ++ digit (modBy 16 int)
    in
        "#" ++ String.concat (List.map channel [ red, green, blue ])


{-| Mix two colors half-and-half in HSL, going the short way around the hue circle
-}
mixHsl : Color -> Color -> Color
mixHsl color1 color2 =
    let
        a =
            Color.toHsla color1

        b =
            Color.toHsla color2

        middle x y =
            (x + y) / 2

        hue =
            middle a.hue b.hue
                + (if abs (a.hue - b.hue) > 0.5 then 0.5 else 0)
    in
        Color.hsla (hue - toFloat (floor hue))
            (middle a.saturation b.saturation)
            (middle a.lightness b.lightness)
            (middle a.alpha b.alpha)
