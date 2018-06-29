module Lifechart.View.Controls exposing (controls, modal)

import Date exposing (Date)
import Time exposing (Time)
import BasicsExtra exposing (roundToPadded)
import DateExtra
import Color.Convert
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lifechart.Model exposing (..)
import Lifechart.Serializer as Serializer
import Lifechart.Demo as Demo


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
            [ style [ "display" => open ], class "modal in" ]
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
            , config model
            , metrics model
            , events model
            , footer
            ]


links : Model -> List (Html Msg)
links model =
    let
        current =
            Serializer.serialize model
    in
        [ ul [ class "nav justify-content-center mb-2" ]
            [ li [ class "nav-item" ]
                [ a [ class "nav-link active", href "#" ] [ text "blank chart" ] ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link", href <| "#" ++ Demo.base64 ] [ text "demo chart" ] ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link", href <| "#" ++ current ] [ text "bookmark your own chart" ] ]
            ]
        ]


circled : Attribute Msg
circled =
    style
        [ ( "background", "#D3D7CF" )
        , ( "border-radius", "1rem" )
        , ( "padding", "0 0.35rem" )
        , ( "margin-left", "0.2rem" )
        ]


config : Model -> List (Html Msg)
config model =
    [ div [ class "row form-group" ]
        [ label [ class "col-5 col-form-label col-form-label-lg" ] [ text "Date of Birth" ]
        , div [ class "col-7" ]
            [ input
                (List.append dateInputAttributes
                    [ class "form-control form-control-lg"
                    , value model.birthDateString
                    , Html.Attributes.max <| DateExtra.toISOString <| Date.fromTime model.now
                    , onInput NewBirthDate
                    ]
                )
                []
            ]
        ]
    , div [ class "row form-group" ]
        [ label [ class "col-5 col-form-label col-form-label-lg" ]
            [ text "Life Expectancy "
            , a
                [ href
                    "https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy"
                , target "_blank"
                , circled
                ]
                [ text "?" ]
            ]
        , div [ class "col-7" ]
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
    , div [ class "row form-group" ]
        [ label [ class "col-5 col-form-label col-form-label-lg" ]
            [ text "Non-Adult Years "
            , a
                [ href
                    "https://www.reddit.com/r/GetMotivated/comments/1vyf9r/made_for_myself_thought_of_you_weeks_left/cexas8u/"
                , target "_blank"
                , circled
                ]
                [ text "?" ]
            ]
        , div [ class "col-7" ]
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


newEvent : Model -> List (Html Msg)
newEvent model =
    [ li [ class "list-group-item" ]
        [ h5 [ class "mb-0" ]
            [ span [] [ text "Events" ]
            , a
                [ href "javascript:void(0)"
                , onClick (ShowEventForm 0)
                , class "float-right"
                ]
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

        newEvent =
            index == 0

        delete =
            a
                [ class "btn btn-danger"
                , href "javascript:void(0)"
                , onClick DeleteEvent
                ]
                [ text "Delete" ]

        submit =
            input
                [ class "btn btn-primary ml-1"
                , type_ "submit"
                , value
                    (if newEvent then
                        "Add"
                     else
                        "Save"
                    )
                ]
                []

        actions =
            if newEvent then
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
                            , style [ "line-height" => "2.5rem" ]
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
                                    ++ if event.overlay then
                                        " d-none"
                                       else
                                        ""
                            , type_ "color"
                            , required True
                            , value event.color
                            , style [ "height" => "2.5rem" ]
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
                            DateExtra.toISOString event.from
                                ++ " to "
                                ++ DateExtra.toISOString event.to
                                ++ " ("
                                ++ eventPercentage event model
                                ++ "%)"
                        ]
                    , div [ class "col-2 text-right" ]
                        [ a
                            [ href "javascript:void(0)"
                            , onClick (ShowEventForm index)
                            ]
                            [ text "edit" ]
                        ]
                    ]
                ]
            , eventForm model index
            ]

        eventIcon event =
            div
                [ class "float-left"
                , style
                    [ "width" => "1rem"
                    , "height" => "1rem"
                    , "line-height" => "1rem"
                    , "font-weight" => "bold"
                    , "color" => "#FFFFFF"
                    , "margin" => "3px 0.5rem 0 0"
                    , "padding"
                        => if event.overlay then
                            "0 0 3px 3px"
                           else
                            "0"
                    , "background-color"
                        => if event.overlay then
                            "#000000"
                           else
                            Color.Convert.colorToHex event.color
                    ]
                ]
                [ text <|
                    if event.overlay then
                        eventInitial event
                    else
                        ""
                ]

        eventPercentage event model =
            100
                * (Date.toTime event.to - Date.toTime event.from)
                / (Date.toTime (relativeDeathDate model) - Date.toTime (relativeBirthDate model))
                |> roundToPadded 1

        list =
            if List.isEmpty events then
                [ li [ class "list-group-item" ]
                    [ div [ class "text-muted text-center" ] [ text "no events yet" ]
                    ]
                ]
            else
                List.concat <|
                    List.indexedMap
                        (\index -> \event -> makeEvent event (index + 1))
                        events

        events =
            model.events
    in
        [ div [ class "card mt-4 mb-4" ]
            [ ul [ class "list-group list-group-flush" ] <|
                List.concat [ newEvent model, list ]
            ]
        ]


metrics : Model -> List (Html Msg)
metrics model =
    let
        left =
            weeksLeft model

        percentage =
            100
                * (model.now - Date.toTime (relativeBirthDate model))
                / (Date.toTime (relativeDeathDate model) - Date.toTime (relativeBirthDate model))
                |> clamp 0 100
                |> roundToPadded 6
    in
        [ div [ class "row mt-2" ]
            [ div [ class "col" ]
                [ h4 [] [ span [ class "badge badge-primary" ] [ text <| percentage ++ "% elapsed" ] ]
                ]
            , div [ class "col text-right" ]
                [ h4 [] [ span [ class "badge badge-primary" ] [ text <| left ++ " weeks left" ] ]
                ]
            ]
        , div [ class "row mt-1" ]
            [ div [ class "col-12" ]
                [ div
                    [ class "progress"
                    , style [ "height" => "1.7rem" ]
                    ]
                    [ div
                        [ class "progress-bar progress-bar-striped progress-bar-animated"
                        , style [ "width" => (percentage ++ "%") ]
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
                ]
            , p [ class "text-center" ]
                [ text "Inspired by "
                , a [ href "https://i.imgur.com/67aHKhF.jpg", target "_blank" ] [ text "this chart" ]
                , text " - "
                , a [ href "http://waitbutwhy.com/2014/05/life-weeks.html", target "_blank" ] [ text "Terrified?" ]
                , text " - "
                , a [ href "https://github.com/infertux/lifechart", target "_blank" ] [ text "Source code" ]
                , text " - "
                , a [ href "javascript:void(0)", onClick ToggleModal ] [ text "Export raw data" ]
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


weeksLeft : Model -> String
weeksLeft model =
    let
        left =
            Date.toTime (relativeDeathDate model) - model.now

        weeks =
            (Time.inHours left) / 24 / 7 |> Basics.max 0
    in
        roundToPadded 6 weeks
