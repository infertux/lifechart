module Lifechart.View.Canvas exposing (canvas)

import Collage exposing (Collage, LineStyle)
import Collage.Render
import Collage.Text as Text exposing (Text)
import Color exposing (Color)
import ColorExtra
import Date
import DateExtra
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Lifechart.Model as Model exposing (..)
import Time


type alias Form =
    Collage Msg


{-| Milliseconds since the Unix epoch
-}
type alias Time =
    Float


fontSize : Int
fontSize =
    12


{-| SVG centers text on its x-height whereas the old canvas renderer centered
it on the em box, so nudge it down a bit to keep the text aligned with the grid
-}
renderText : Int -> Text -> Form
renderText size text =
    Collage.group
        [ Text.size size text
            |> Collage.rendered
            |> Collage.shiftY (-0.25 * toFloat size)
        ]


lineHeight : number
lineHeight =
    16


canvasHeight : Float -> Float
canvasHeight lifeExpectancy =
    lifeExpectancy * (weekWidth + weekBorder * 2)


canvasWidth : number
canvasWidth =
    indexWidth + 52 * (weekWidth + weekBorder * 2) + legendWidth


indexWidth : number
indexWidth =
    60


legendWidth : number
legendWidth =
    20


weekWidth : number
weekWidth =
    10


weekBorder : number
weekBorder =
    1


oneDay : number
oneDay =
    86400 * 1000


oneWeek : number
oneWeek =
    oneDay * 7


{-| Takes the chart's inputs one by one so that `Html.Lazy` can skip redrawing
the chart when none of them changed
-}
canvas : Date.Date -> Int -> Int -> Int -> Bool -> List Event -> Int -> Html Msg
canvas birthDate kidUntil oldFrom lifeExpectancy onlyAdultYears events nowMillis =
    draw
        { birthDate = birthDate
        , kidUntil = kidUntil
        , oldFrom = oldFrom
        , lifeExpectancy = lifeExpectancy
        , onlyAdultYears = onlyAdultYears
        , events = events
        , now = Time.millisToPosix nowMillis
        }


draw : Chart {} -> Html Msg
draw model =
    let
        ( from, to ) =
            yearRange model

        height =
            lineHeight * 2 + canvasHeight (toFloat (to - from))

        grid =
            Collage.group (years model)
                |> Collage.shiftX (-canvasWidth / 2)
                |> Collage.shiftY (height / 2)

        theLegend =
            (if model.onlyAdultYears then
                adultLegend model

             else
                legend model
            )
                |> Collage.shiftX (canvasWidth / 2 - legendWidth + lineHeight)
    in
    svgBox ( canvasWidth, toFloat (ceiling height) ) (Collage.group [ grid, theLegend ])


{-| The years drawn on the chart, both ends included
-}
yearRange : Chart {} -> ( Int, Int )
yearRange model =
    if model.onlyAdultYears then
        ( model.kidUntil, maxOldFrom model )

    else
        ( 0, model.lifeExpectancy )


{-| Same as `Collage.Render.svgBox` but with a `viewBox` so that the chart can
be scaled down to fit narrow screens
-}
svgBox : ( Float, Float ) -> Form -> Html Msg
svgBox ( width, height ) form =
    let
        ( w, h ) =
            ( String.fromFloat width, String.fromFloat height )
    in
    form
        |> Collage.shift ( width / 2, -height / 2 )
        |> Collage.Render.svgExplicit
            [ attribute "width" w
            , attribute "height" h
            , attribute "viewBox" ("0 0 " ++ w ++ " " ++ h)
            , attribute "version" "1.1"
            ]


legend : Chart {} -> Form
legend model =
    let
        makeText string =
            Text.fromString string
                |> renderText fontSize
                |> Collage.rotate (degrees -90)

        ( textOffset, markOffset ) =
            ( 11, 6 )

        ( kidUntil, oldFrom, lifeExpectancy ) =
            ( toFloat model.kidUntil
            , toFloat model.oldFrom
            , toFloat model.lifeExpectancy
            )

        kidUntilMax =
            Model.maxKidUntil model |> toFloat

        kid =
            makeText "kid"
                |> Collage.shiftY
                    (canvasHeight ((lifeExpectancy - kidUntilMax) / 2) - textOffset)

        kidMark =
            Collage.rectangle 10 1
                |> Collage.filled (Collage.uniform Color.black)
                |> Collage.shiftX -2
                |> Collage.shiftY
                    (canvasHeight (lifeExpectancy / 2 - kidUntil) - markOffset)

        oldFromMax =
            Model.maxOldFrom model |> toFloat

        adult =
            if oldFromMax - kidUntil < 8 then
                makeText ""

            else
                makeText "adult years"
                    |> Collage.shiftY
                        (canvasHeight (lifeExpectancy / 2 - (kidUntil + oldFromMax) / 2) - textOffset)

        oldMark =
            Collage.rectangle 10 1
                |> Collage.filled (Collage.uniform Color.black)
                |> Collage.shiftX -2
                |> Collage.shiftY
                    -(canvasHeight (oldFrom - lifeExpectancy / 2) + markOffset)

        old =
            if lifeExpectancy - oldFrom < 1 then
                makeText ""

            else
                makeText "old"
                    |> Collage.shiftY
                        -(canvasHeight ((lifeExpectancy + oldFrom) / 2 - lifeExpectancy / 2) + textOffset)
    in
    Collage.group [ kid, kidMark, adult, oldMark, old ]


{-| With non-adult years hidden, the whole chart is adult years so a single
label says which ages it covers
-}
adultLegend : Chart {} -> Form
adultLegend model =
    Text.fromString
        ("adult years ("
            ++ String.fromInt model.kidUntil
            ++ "–"
            ++ String.fromInt (maxOldFrom model)
            ++ ")"
        )
        |> renderText fontSize
        |> Collage.rotate (degrees -90)
        |> Collage.shiftY -11


years : Chart {} -> List Form
years model =
    let
        makeYear i =
            Collage.group (year model i)
                |> Collage.shiftY
                    ((0 - toFloat (i - from + 1)) * (weekWidth + weekBorder * 2) - lineHeight)

        ( from, to ) =
            yearRange model
    in
    weekIndexes :: (List.range from to |> List.map makeYear)


weekIndexes : Form
weekIndexes =
    let
        makeIndex i =
            Text.fromString (String.fromInt i)
                |> renderText fontSize
                |> Collage.shiftX (toFloat i * (weekWidth + weekBorder * 2))
    in
    List.range 1 52
        |> List.filter (\i -> modBy 2 i /= 0)
        |> List.map makeIndex
        |> Collage.group
        |> Collage.shift ( indexWidth, -10 )


year : Chart {} -> Int -> List Form
year model index =
    let
        calendarYear =
            Date.year model.birthDate + index

        paddedIndex =
            String.padLeft 2 '0' (String.fromInt index)

        label =
            Text.fromString (String.fromInt calendarYear ++ " / " ++ paddedIndex)
                |> renderText fontSize
                |> Collage.shift ( 32, 2 )

        makeWeek i =
            week model index i
                |> Collage.shiftX
                    (toFloat i * (weekWidth + weekBorder * 2) + indexWidth)
    in
    label :: (List.range 1 52 |> List.map makeWeek)


week : Chart {} -> Int -> Int -> Form
week model yearIndex weekIndex =
    let
        time =
            yearWeekToTime model ( yearIndex, weekIndex )

        match event =
            (DateExtra.toTime event.from <= time) && (DateExtra.toTime event.to >= time)

        ( overlays, events ) =
            List.filter match model.events |> List.partition .overlay

        weekColor =
            List.map .color events |> mixColors

        initial =
            List.head overlays |> Maybe.andThen makeInitial

        makeInitial event =
            Just
                (eventInitial event
                    |> Text.fromString
                    |> Text.typeface Text.Monospace
                    |> Text.weight Text.Bold
                    |> Text.color Color.white
                    |> renderText weekWidth
                    |> Collage.shiftY 1
                )

        square color =
            Collage.square weekWidth |> Collage.filled (Collage.uniform color)

        filled color =
            case initial of
                Nothing ->
                    square color

                Just initialForm ->
                    Collage.group [ initialForm, square color ]

        outlined =
            Collage.square (weekWidth - weekBorder) |> Collage.outlined lineStyle
    in
    if isCurrentWeek model time then
        filled Color.black |> Collage.opacity 0.5

    else
        case weekColor of
            Nothing ->
                if outOfBounds model time then
                    filled Color.lightGrey

                else if isKid model time || isOld model time then
                    filled Color.grey

                else if isPast model time then
                    filled Color.black

                else
                    outlined

            Just color ->
                filled color


mixColors : List Color -> Maybe Color
mixColors colors =
    case colors of
        [] ->
            Nothing

        firstColor :: otherColors ->
            Just <|
                List.foldl
                    (\oldColor -> \newColor -> ColorExtra.mixHsl oldColor newColor)
                    firstColor
                    otherColors


outOfBounds : Chart {} -> Time -> Bool
outOfBounds model time =
    (time < DateExtra.toTime (relativeBirthDate model))
        || (time > DateExtra.toTime (relativeDeathDate model))


isCurrentWeek : Chart {} -> Time -> Bool
isCurrentWeek model time =
    let
        now =
            nowTime model

        halfWeek =
            oneWeek / 2
    in
    now - halfWeek <= time && time < now + halfWeek


isPast : Chart {} -> Time -> Bool
isPast model time =
    time < nowTime model


isKid : Chart {} -> Time -> Bool
isKid model time =
    let
        kidDate =
            partialDate model (Date.year (relativeBirthDate model) + model.kidUntil)
    in
    if model.onlyAdultYears then
        False

    else
        DateExtra.toTime kidDate >= time


isOld : Chart {} -> Time -> Bool
isOld model time =
    let
        oldDate =
            partialDate model (Date.year (relativeBirthDate model) + model.oldFrom)
    in
    if model.onlyAdultYears then
        False

    else
        DateExtra.toTime oldDate <= time


yearWeekToTime : Chart {} -> ( Int, Int ) -> Time
yearWeekToTime model ( yearIndex, weekIndex ) =
    let
        currentYear =
            Date.year model.birthDate + yearIndex

        beginningOfYear =
            Date.fromCalendarDate currentYear Time.Jan 1

        firstMondayOffset =
            Date.weekdayNumber beginningOfYear - 1

        yearOffset =
            toFloat (weekIndex - 1) * oneWeek + toFloat firstMondayOffset * oneDay
    in
    DateExtra.toTime beginningOfYear + yearOffset


lineStyle : LineStyle
lineStyle =
    Collage.solid weekBorder (Collage.uniform Color.black)
