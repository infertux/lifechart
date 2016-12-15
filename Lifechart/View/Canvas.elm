module Lifechart.View.Canvas exposing (canvas)

import Html exposing (..)
import Collage exposing (Form, LineStyle)
import Element exposing (Element)
import Text exposing (Text)
import Color exposing (Color)
import Color.Interpolate
import Date exposing (Date)
import Time exposing (Time)
import DateExtra
import Lifechart.Model exposing (..)


lineHeight : number
lineHeight =
    16


canvasHeight : number -> Float
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
    9


weekBorder : number
weekBorder =
    1


oneDay : number
oneDay =
    86400 * 1000


oneWeek : number
oneWeek =
    oneDay * 7


canvas : Model -> Html Msg
canvas model =
    let
        height =
            lineHeight * 2 + canvasHeight model.lifeExpectancy

        grid =
            Collage.group (years model)
                |> Collage.moveX (-canvasWidth / 2)
                |> Collage.moveY (height / 2)

        theLegend =
            legend model
                |> Collage.moveX (canvasWidth / 2 - legendWidth + lineHeight)

        form =
            Collage.group [ grid, theLegend ]
    in
        Collage.collage canvasWidth (ceiling height) [ form ] |> Element.toHtml


legend : Model -> Form
legend model =
    let
        makeText text =
            Text.fromString text
                |> Collage.text
                |> Collage.rotate (degrees -90)

        ( textOffset, markOffset ) =
            ( 11, 6 )

        ( kidUntil, oldFrom, lifeExpectancy ) =
            ( toFloat model.kidUntil
            , toFloat model.oldFrom
            , toFloat model.lifeExpectancy
            )

        maxKidUntil =
            Basics.min lifeExpectancy kidUntil

        kid =
            makeText "kid"
                |> Collage.moveY
                    (canvasHeight ((lifeExpectancy - maxKidUntil) / 2) - textOffset)

        kidMark =
            Collage.rect 10 1
                |> Collage.filled Color.black
                |> Collage.moveX -2
                |> Collage.moveY
                    (canvasHeight (lifeExpectancy / 2 - kidUntil) - markOffset)

        maxOldFrom =
            Basics.min lifeExpectancy oldFrom

        productive =
            if maxOldFrom - kidUntil < 8 then
                makeText ""
            else
                makeText "productive years"
                    |> Collage.moveY
                        (canvasHeight (lifeExpectancy / 2 - (kidUntil + maxOldFrom) / 2) - textOffset)

        oldMark =
            Collage.rect 10 1
                |> Collage.filled Color.black
                |> Collage.moveX -2
                |> Collage.moveY
                    -(canvasHeight (oldFrom - lifeExpectancy / 2) + markOffset)

        old =
            if lifeExpectancy - oldFrom < 1 then
                makeText ""
            else
                makeText "old"
                    |> Collage.moveY
                        -(canvasHeight ((lifeExpectancy + oldFrom) / 2 - lifeExpectancy / 2) + textOffset)
    in
        Collage.group [ kid, kidMark, productive, oldMark, old ]


years : Model -> List Form
years model =
    let
        makeYear i =
            Collage.group (year model i)
                |> Collage.moveY
                    ((toFloat -i - 1) * (weekWidth + weekBorder * 2) - lineHeight)
    in
        weekIndexes :: (List.range 0 model.lifeExpectancy |> List.map makeYear)


weekIndexes : Form
weekIndexes =
    let
        makeIndex i =
            Text.fromString (toString i)
                |> Collage.text
                |> Collage.moveX (toFloat i * (weekWidth + weekBorder * 2))
    in
        List.range 1 52
            |> List.filter (\i -> i % 2 /= 0)
            |> List.map makeIndex
            |> Collage.group
            |> Collage.move ( indexWidth, -10 )


year : Model -> Int -> List Form
year model index =
    let
        year =
            Date.year model.birthDate + index

        paddedIndex =
            String.padLeft 2 '0' (toString index)

        label =
            Text.fromString (toString year ++ " / " ++ paddedIndex)
                |> Collage.text
                |> Collage.move ( 32, 2 )

        makeWeek i =
            week model index i
                |> Collage.moveX
                    (toFloat i * (weekWidth + weekBorder * 2) + indexWidth)
    in
        label :: (List.range 1 52 |> List.map makeWeek)


week : Model -> Int -> Int -> Form
week model year week =
    let
        time =
            yearWeekToTime model ( year, week )

        match event =
            (Date.toTime event.from <= time)
                && (Date.toTime event.to >= time)

        events =
            List.filter match model.events

        colors =
            List.map .color events

        weekColor =
            List.head colors |> Maybe.andThen mixColors

        mixColors firstColor =
            Just <|
                List.foldl
                    (\oldColor ->
                        \newColor ->
                            Color.Interpolate.interpolate
                                Color.Interpolate.HSL
                                oldColor
                                newColor
                                0.5
                    )
                    firstColor
                    (List.drop 1 colors)

        filled color =
            Collage.square weekWidth |> Collage.filled color

        outlined =
            Collage.square (weekWidth - weekBorder) |> Collage.outlined lineStyle
    in
        if isCurrentWeek model time then
            filled Color.black |> Collage.alpha 0.5
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


outOfBounds : Model -> Time -> Bool
outOfBounds model time =
    time < Date.toTime model.birthDate || time > Date.toTime (deathDate model)


isCurrentWeek : Model -> Time -> Bool
isCurrentWeek model time =
    let
        now =
            model.now

        halfWeek =
            oneWeek / 2
    in
        now - halfWeek <= time && time < now + halfWeek


isPast : Model -> Time -> Bool
isPast model time =
    time < model.now


isKid : Model -> Time -> Bool
isKid model time =
    let
        kidDate =
            partialDate model (Date.year model.birthDate + model.kidUntil)
    in
        Date.toTime kidDate >= time


isOld : Model -> Time -> Bool
isOld model time =
    let
        oldDate =
            partialDate model (Date.year model.birthDate + model.oldFrom)
    in
        Date.toTime oldDate <= time


yearWeekToTime : Model -> ( Int, Int ) -> Time
yearWeekToTime model ( year, week ) =
    let
        currentYear =
            Date.year model.birthDate + year

        beginningOfYear =
            DateExtra.unsafeFromString <| toString currentYear ++ "-01-01"

        firstMondayOffset =
            case Date.dayOfWeek beginningOfYear of
                Date.Mon ->
                    0

                Date.Tue ->
                    1

                Date.Wed ->
                    2

                Date.Thu ->
                    3

                Date.Fri ->
                    4

                Date.Sat ->
                    5

                Date.Sun ->
                    6

        yearOffset =
            toFloat (week - 1) * oneWeek + toFloat firstMondayOffset * oneDay
    in
        Date.toTime beginningOfYear + yearOffset


lineStyle : LineStyle
lineStyle =
    let
        default =
            Collage.defaultLine
    in
        { default | width = weekBorder }
