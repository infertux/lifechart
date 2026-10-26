module Lifechart.View.Controls exposing (controls, footer, modal)

import BasicsExtra exposing (roundToPadded)
import ColorExtra
import Date
import DateExtra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lifechart.Demo as Demo
import Lifechart.Model exposing (..)
import Lifechart.Serializer as Serializer


modal : Model -> Html Msg
modal model =
    let
        open =
            if model.modalOpen then
                "block"

            else
                "none"

        json =
            Serializer.serializeJson model
    in
    div
        [ style "display" open, class "modal in" ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-body" ]
                    [ div
                        [ class "mb-1" ]
                        [ text "You can copy/paste JSON below to export/import data." ]
                    , textarea
                        [ class "form-control"
                        , rows 20
                        , onInput NewConfig
                        ]
                        [ text json ]
                    ]
                , div [ class "modal-footer" ]
                    [ button
                        [ type_ "button"
                        , class "btn btn-primary"
                        , onClick ToggleModal
                        ]
                        [ text "Close" ]
                    ]
                ]
            ]
        ]


controls : Model -> Html Msg
controls model =
    div [] <|
        List.concat
            [ [ h1 [ class "text-center mb-2" ] [ text "Spend your time wisely." ] ]
            , [ div [ class "text-center text-muted mb-1" ] [ text "This will either inspire you or terrify you - hopefully the former." ] ]
            , links model
            , editOnly (demoHint model)
            , editOnly (config model)
            , metrics model
            , events model
            , legend model
            ]


{-| Editing is only supported on bigger screens: phones get a read-only view of
the chart so that shared links still look fine
-}
editOnly : List (Html Msg) -> List (Html Msg)
editOnly content =
    [ div [ class "d-none d-md-block" ] content ]


isDemo : Model -> Bool
isDemo model =
    Serializer.deserialize Demo.base64
        |> Result.map (mergeJsonModel model >> Serializer.serialize)
        |> (==) (Ok (Serializer.serialize model))


demoHint : Model -> List (Html Msg)
demoHint model =
    if isDemo model then
        [ div [ class "text-center text-muted mb-2" ]
            [ text "This is a demo chart. "
            , a [ href "#" ] [ text "Start your own blank chart" ]
            , text " or edit this one."
            ]
        ]

    else
        []


links : Model -> List (Html Msg)
links model =
    let
        current =
            Serializer.serialize model

        navLink isActive =
            classList [ ( "nav-link", True ), ( "active", isActive ) ]
    in
    [ ul [ class "nav justify-content-center mb-2" ]
        [ li [ class "nav-item" ]
            [ a [ navLink (current == Serializer.serialize (initialModel model.key)), href "#" ] [ text "blank chart" ] ]
        , li [ class "nav-item" ]
            [ a [ navLink (isDemo model), href <| "#" ++ Demo.base64 ] [ text "demo chart" ] ]
        , li [ class "nav-item d-none d-md-block" ]
            [ span [ class "nav-link text-muted" ] [ text "press Ctrl+D or ⌘D to bookmark your chart" ] ]
        ]
    ]


circled : List (Attribute Msg)
circled =
    [ style "background" "#D3D7CF"
    , style "border-radius" "1rem"
    , style "padding" "0 0.35rem"
    , style "margin-left" "0.2rem"
    ]


{-| A button that looks like a link, for in-page actions (`javascript:` URLs
are blocked by Elm 0.19)
-}
actionLink : Msg -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
actionLink msg attributes =
    button
        ([ type_ "button"
         , class "btn btn-link p-0 border-0 align-baseline"
         , style "font-size" "inherit"
         , style "line-height" "inherit"
         , onClick msg
         ]
            ++ attributes
        )


config : Model -> List (Html Msg)
config model =
    [ div [ class "row form-group" ]
        [ label [ class "col-6 col-form-label col-form-label-lg" ] [ text "Date of Birth" ]
        , div [ class "col-6" ]
            [ input
                (List.append dateInputAttributes
                    [ class "form-control form-control-lg"
                    , value model.birthDateString
                    , Html.Attributes.max <| Date.toIsoString <| today model
                    , onInput NewBirthDate
                    ]
                )
                []
            ]
        ]
    , div [ class "row form-group" ]
        [ label [ class "col-6 col-form-label col-form-label-lg" ]
            [ text "Life Expectancy "
            , a
                ([ href
                    "https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy"
                 , target "_blank"
                 ]
                    ++ circled
                )
                [ text "?" ]
            ]
        , div [ class "col-6" ]
            [ input
                [ class "form-control form-control-lg"
                , type_ "number"
                , required True
                , value model.lifeExpectancyString
                , Html.Attributes.min "1"
                , Html.Attributes.max "500"
                , onInput NewLifeExpectancy
                ]
                []
            ]
        ]
    , adultYears model
    , div [ class "row form-group" ]
        [ label [ class "col-6 col-form-label col-form-label-lg" ]
            [ text "Non-Adult Years "
            , a
                ([ href
                    "https://www.reddit.com/r/GetMotivated/comments/1vyf9r/made_for_myself_thought_of_you_weeks_left/cexas8u/"
                 , target "_blank"
                 ]
                    ++ circled
                )
                [ text "?" ]
            ]
        , div [ class "col-6" ]
            [ label [ class "form-check-inline form-control-lg" ]
                [ input
                    [ class "form-check-input"
                    , type_ "radio"
                    , checked <| not model.onlyAdultYears
                    , onClick <| OnlyAdultYears False
                    ]
                    []
                , text " Show"
                ]
            , label [ class "form-check-inline form-control-lg" ]
                [ input
                    [ class "form-check-input"
                    , type_ "radio"
                    , checked model.onlyAdultYears
                    , onClick <| OnlyAdultYears True
                    ]
                    []
                , text " Hide"
                ]
            ]
        ]
    ]


adultYears : Model -> Html Msg
adultYears model =
    let
        yearInput value_ min_ max_ msg =
            input
                [ class "form-control form-control-lg"
                , type_ "number"
                , required True
                , value value_
                , Html.Attributes.min (String.fromInt min_)
                , Html.Attributes.max (String.fromInt max_)
                , onInput msg
                ]
                []
    in
    div [ class "row form-group" ]
        [ label [ class "col-6 col-form-label col-form-label-lg" ] [ text "Adult Years" ]
        , div [ class "col-6" ]
            [ div [ class "input-group input-group-lg" ]
                [ yearInput model.kidUntilString 0 (model.oldFrom - 1) NewKidUntil
                , div [ class "input-group-append input-group-prepend" ]
                    [ span [ class "input-group-text" ] [ text "to" ] ]
                , yearInput model.oldFromString (model.kidUntil + 1) 500 NewOldFrom
                ]
            ]
        ]


newEvent : Model -> List (Html Msg)
newEvent model =
    [ li [ class "list-group-item" ]
        [ h5 [ class "mb-0" ]
            [ span [] [ text "Events" ]
            , actionLink (ShowEventForm 0)
                [ class "float-right" ]
                [ text "Add event" ]
            ]
        ]
    , eventForm model 0
    ]


eventForm : Model -> Int -> Html Msg
eventForm model index =
    let
        event =
            model.eventForm

        visibility =
            if model.eventFormOpen == index then
                ""

            else
                " d-none"

        isNewEvent =
            index == 0

        delete =
            button
                [ type_ "button"
                , class "btn btn-danger"
                , onClick DeleteEvent
                ]
                [ text "Delete" ]

        submit =
            input
                [ class "btn btn-primary ml-1"
                , type_ "submit"
                , value
                    (if isNewEvent then
                        "Add"

                     else
                        "Save"
                    )
                ]
                []

        actions =
            if isNewEvent then
                [ submit ]

            else
                [ delete, submit ]
    in
    li [ class <| "list-group-item" ++ visibility ]
        [ Html.form [ onSubmit SaveEvent ]
            [ div [ class "row form-group" ]
                [ div [ class "col" ]
                    [ div [ class "input-group" ]
                        [ div [ class "input-group-prepend" ]
                            [ div [ class "input-group-text" ] [ text "From" ]
                            ]
                        , input
                            (List.append dateInputAttributes
                                [ class "form-control"
                                , value event.from
                                , Html.Attributes.max event.to
                                , onInput (UpdateEvent EventFrom)
                                ]
                            )
                            []
                        ]
                    ]
                , div [ class "col" ]
                    [ div [ class "input-group" ]
                        [ div [ class "input-group-prepend" ]
                            [ div [ class "input-group-text" ] [ text "To" ]
                            ]
                        , input
                            (List.append dateInputAttributes
                                [ class "form-control"
                                , value event.to
                                , Html.Attributes.min event.from
                                , onInput (UpdateEvent EventTo)
                                ]
                            )
                            []
                        ]
                    ]
                ]
            , div [ class "row form-group" ]
                [ div [ class "col" ]
                    [ div [ class "input-group" ]
                        [ div [ class "input-group-prepend" ]
                            [ div [ class "input-group-text" ] [ text "Label" ]
                            ]
                        , input
                            [ class "form-control"
                            , placeholder "label"
                            , type_ "text"
                            , required True
                            , value event.label
                            , onInput (UpdateEvent EventLabel)
                            ]
                            []
                        ]
                    ]
                , div [ class "col" ]
                    [ label
                        [ class "form-check-inline"
                        , style "line-height" "2.5rem"
                        ]
                        [ input
                            [ class "form-check-input"
                            , type_ "checkbox"
                            , checked event.overlay
                            , onCheck UpdateEventOverlay
                            ]
                            []
                        , text " overlay"
                        ]
                    , input
                        [ class <|
                            "float-right"
                                ++ (if event.overlay then
                                        " d-none"

                                    else
                                        ""
                                   )
                        , type_ "color"
                        , required True
                        , value event.color
                        , style "height" "2.5rem"
                        , onInput (UpdateEvent EventColor)
                        ]
                        []
                    ]
                ]
            , div [ class "row form-group" ]
                [ div [ class "col-12 text-right" ] actions
                ]
            ]
        ]


events : Model -> List (Html Msg)
events model =
    let
        makeEvent event index =
            [ li [ class "list-group-item" ]
                [ div
                    [ class "row" ]
                    [ div [ class "col-3" ]
                        [ eventIcon event
                        , span [] [ text event.label ]
                        ]
                    , div [ class "col-7 text-right text-muted" ]
                        [ text <|
                            Date.toIsoString event.from
                                ++ " to "
                                ++ Date.toIsoString event.to
                                ++ " ("
                                ++ eventPercentage event
                                ++ "%)"
                        ]
                    , div [ class "col-2 text-right" ]
                        [ actionLink (ShowEventForm index) [] [ text "edit" ]
                        ]
                    ]
                ]
            , eventForm model index
            ]

        eventPercentage event =
            100
                * (DateExtra.toTime event.to - DateExtra.toTime event.from)
                / (DateExtra.toTime (relativeDeathDate model) - DateExtra.toTime (relativeBirthDate model))
                |> roundToPadded 1

        list =
            if List.isEmpty model.events then
                [ li [ class "list-group-item" ]
                    [ div [ class "text-muted text-center" ] [ text "no events yet" ]
                    ]
                ]

            else
                List.concat <|
                    List.indexedMap
                        (\index -> \event -> makeEvent event (index + 1))
                        model.events
    in
    [ div [ class "card mt-4 mb-4 d-none d-md-flex" ]
        [ ul [ class "list-group list-group-flush" ] <|
            List.concat [ newEvent model, list ]
        ]
    ]


{-| Phones get a compact legend of the events instead of the editable list
-}
legend : Model -> List (Html Msg)
legend model =
    if List.isEmpty model.events then
        []

    else
        [ div [ class "d-md-none text-center mt-3" ] <|
            List.map
                (\event ->
                    span [ class "d-inline-block text-nowrap mx-2" ]
                        [ eventIcon event, text event.label ]
                )
                model.events
        ]


eventIcon : Event -> Html Msg
eventIcon event =
    div
        [ class "float-left"
        , style "width" "1rem"
        , style "height" "1rem"
        , style "line-height" "1rem"
        , style "font-weight" "bold"
        , style "color" "#FFFFFF"
        , style "margin" "3px 0.5rem 0 0"
        , style "padding" <|
            if event.overlay then
                "0 0 3px 3px"

            else
                "0"
        , style "background-color" <|
            if event.overlay then
                "#000000"

            else
                ColorExtra.toHex event.color
        ]
        [ text <|
            if event.overlay then
                eventInitial event

            else
                ""
        ]


metrics : Model -> List (Html Msg)
metrics model =
    let
        left =
            weeksLeft model

        percentage =
            100
                * (nowTime model - DateExtra.toTime (relativeBirthDate model))
                / (DateExtra.toTime (relativeDeathDate model) - DateExtra.toTime (relativeBirthDate model))
                |> clamp 0 100
                |> roundToPadded 2

        -- with non-adult years hidden the metrics only count adult years, so
        -- say so rather than pass them off as the whole life
        ( elapsedLabel, leftLabel ) =
            if model.onlyAdultYears then
                ( "% of adult years elapsed", " adult weeks left" )

            else
                ( "% elapsed", " weeks left" )
    in
    [ -- side by side when they fit, otherwise the second one wraps under the
      -- first and both stay left-aligned (centered on phones)
      div [ class "d-flex flex-column flex-sm-row flex-sm-wrap align-items-center justify-content-sm-between mt-2" ]
        [ h4 [ class "mr-sm-3" ] [ span [ class "badge badge-primary" ] [ text <| percentage ++ elapsedLabel ] ]
        , h4 [] [ span [ class "badge badge-primary" ] [ text <| left ++ leftLabel ] ]
        ]
    , div [ class "row mt-1" ]
        [ div [ class "col-12" ]
            [ div
                [ class "progress"
                , style "height" "1.7rem"
                ]
                [ -- not progress-bar-animated: moving stripes repaint the whole
                  -- chart on every frame, which keeps the CPU busy
                  div
                    [ class "progress-bar progress-bar-striped"
                    , style "width" (percentage ++ "%")
                    ]
                    []
                ]
            ]
        ]
    ]


footer : List (Html Msg)
footer =
    [ div [ class "row mt-1" ]
        [ div [ class "col-12" ]
            [ div [ class "alert alert-info" ]
                [ strong [] [ text "Privacy: " ]
                , text "this page runs entirely in your browser so no data is sent to any server."
                , span [ class "d-none d-md-inline" ]
                    [ text " The catch is that your chart is saved in the page address, so press Ctrl+D or ⌘D to bookmark it again after every change." ]
                ]
            , p [ class "text-center" ]
                [ text "Inspired by "
                , a [ href "https://i.imgur.com/67aHKhF.jpg", target "_blank" ] [ text "this chart" ]
                , text " - "
                , a [ href "http://waitbutwhy.com/2014/05/life-weeks.html", target "_blank" ] [ text "Terrified?" ]
                , text " - "
                , a [ href "https://github.com/infertux/lifechart", target "_blank" ] [ text "Source code" ]
                , span [ class "d-none d-md-inline" ]
                    [ text " - "
                    , actionLink ToggleModal [] [ text "Export raw data" ]
                    ]
                ]
            , div [ class "d-none" ]
                [ strong [] [ text "Reminder: " ]
                , text "every single person you've ever known will die eventually - and so will you - but "
                , a [ href "https://youtu.be/k5RH3BdXDOY", target "_blank" ] [ text "don't worry" ]
                , text " :)"
                ]
            ]
        ]
    ]


dateInputAttributes : List (Attribute Msg)
dateInputAttributes =
    [ type_ "date"
    , required True
    , placeholder "YYYY-MM-DD"
    , pattern "[1-2]\\d{3}-[0-1]\\d-[0-3]\\d"
    ]


{-| Six decimals because a millionth of a week is 0.6 seconds, so the last digit
changes on every tick to show that the clock is ticking
-}
weeksLeft : Model -> String
weeksLeft model =
    let
        left =
            DateExtra.toTime (relativeDeathDate model) - nowTime model

        weeks =
            left / (7 * 24 * 60 * 60 * 1000) |> Basics.max 0
    in
    roundToPadded 6 weeks
