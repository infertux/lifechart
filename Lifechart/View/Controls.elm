module Lifechart.View.Controls exposing (controls, modal)

import Date exposing (Date)
import Time exposing (Time)
import BasicsExtra exposing (roundToPadded)
import DateExtra
import Color.Convert
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
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
            [ style [ ( "display", open ) ], class "modal in" ]
            [ div [ class "modal-dialog" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-body" ]
                        [ textarea
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
            [ [ h1 [ class "text-xs-center mb-1" ] [ text "Spend your time wisely." ] ]
            , [ div [ class "text-xs-center text-muted mb-1" ] [ text "This will either inspire you or terrify you - hopefully the former." ] ]
            , links model
            , config model
            , metrics model
            , events model
            , footer
            ]


links : Model -> List (Html Msg)
links model =
    let
        demo =
            "ewogICJiaXJ0aC1kYXRlIjogIjE5ODgtMDctMjQiLAogICJsaWZlLWV4cGVjdGFuY3kiOiA4MCwKICAia2lkLXVudGlsIjogMTgsCiAgIm9sZC1mcm9tIjogNzAsCiAgImV2ZW50cyI6IFsKICAgIHsKICAgICAgImZyb20iOiAiMjAxMC0wOS0xNCIsCiAgICAgICJ0byI6ICIyMDE0LTAyLTAxIiwKICAgICAgImNvbG9yIjogIiNmNTc5MDAiLAogICAgICAibGFiZWwiOiAiY29sbGVnZSIKICAgIH0sCiAgICB7CiAgICAgICJmcm9tIjogIjIwMTUtMDYtMDEiLAogICAgICAidG8iOiAiMjAxNi0wNS0zMSIsCiAgICAgICJjb2xvciI6ICIjNzNkMjE2IiwKICAgICAgImxhYmVsIjogInRyaXAiCiAgICB9CiAgXQp9Cg=="

        current =
            Serializer.serialize model
    in
        [ nav [ class "nav nav-inline text-xs-center mb-1" ]
            [ a [ class "nav-link", href "#" ] [ text "blank chart" ]
            , a [ class "nav-link", href <| "#" ++ demo ] [ text "demo chart" ]
            , a [ class "nav-link", href <| "#" ++ current ] [ text "bookmark your own chart" ]
            ]
        ]


config : Model -> List (Html Msg)
config model =
    [ div [ class "row form-group" ]
        [ label [ class "col-xs-4 col-form-label col-form-label-lg" ] [ text "Date of Birth" ]
        , div [ class "col-xs-8" ]
            [ input
                (List.append dateInputAttributes
                    [ class "form-control form-control-lg"
                    , value model.birthDateString
                    , onInput NewDateOfBirth
                    ]
                )
                []
            ]
        ]
    , div [ class "row form-group" ]
        [ label [ class "col-xs-4 col-form-label col-form-label-lg" ] [ text "Life Expectancy" ]
        , div [ class "col-xs-8" ]
            [ input
                [ class "form-control form-control-lg"
                , type_ "number"
                , required True
                , value <| toString model.lifeExpectancy
                , onInput NewLifeExpectancy
                ]
                []
            ]
        ]
    ]


newEvent : Model -> List (Html Msg)
newEvent model =
    let
        visibility =
            if model.newEventOpen then
                ""
            else
                " hidden-xs-up"

        inputs =
            [ div [ class "row form-group" ]
                [ div [ class "col-xs-6" ]
                    [ div [ class "input-group" ]
                        [ span [ class "input-group-addon" ] [ text "From" ]
                        , input
                            (List.append dateInputAttributes
                                [ class "form-control"
                                , onInput (UpdateNewEvent EventFrom)
                                ]
                            )
                            []
                        ]
                    ]
                , div [ class "col-xs-6" ]
                    [ div [ class "input-group" ]
                        [ span [ class "input-group-addon" ] [ text "To" ]
                        , input
                            (List.append dateInputAttributes
                                [ class "form-control"
                                , onInput (UpdateNewEvent EventTo)
                                ]
                            )
                            []
                        ]
                    ]
                ]
            , div [ class "row form-group" ]
                [ div [ class "col-xs-6" ]
                    [ div [ class "input-group" ]
                        [ span [ class "input-group-addon" ] [ text "Label" ]
                        , input
                            [ class "form-control"
                            , type_ "text"
                            , required True
                            , onInput (UpdateNewEvent EventLabel)
                            ]
                            []
                        ]
                    ]
                , div [ class "col-xs-3" ]
                    [ div [ class "input-group" ]
                        [ span [ class "input-group-addon" ] [ text "Color" ]
                        , input
                            [ class "form-control"
                            , type_ "color"
                            , required True
                            , style [ ( "height", "2.5rem" ) ]
                            , onInput (UpdateNewEvent EventColor)
                            ]
                            []
                        ]
                    ]
                , div [ class "col-xs-3 text-xs-right" ]
                    [ input
                        [ class "btn btn-primary"
                        , type_ "submit"
                        , value "Add"
                        ]
                        []
                    ]
                ]
            ]
    in
        [ li [ class "list-group-item" ]
            [ h5 [ class "mb-0" ]
                [ span [] [ text "Events" ]
                , a
                    [ href "javascript:void(0)"
                    , onClick ToggleNewEvent
                    , class "float-xs-right"
                    ]
                    [ text "Add event" ]
                ]
            ]
        , li [ class <| "list-group-item" ++ visibility ]
            [ Html.form [ onSubmit SaveNewEvent ] inputs
            ]
        ]


events : Model -> List (Html Msg)
events model =
    let
        makeEvent event =
            li [ class "list-group-item" ]
                [ div
                    [ class "float-xs-left"
                    , style
                        [ ( "width", "1rem" )
                        , ( "height", "1rem" )
                        , ( "margin", "3px 0.5rem 0 0" )
                        , ( "background-color", Color.Convert.colorToHex event.color )
                        ]
                    ]
                    []
                , span [] [ text event.label ]
                , span [ class "float-xs-right text-muted" ]
                    [ text <|
                        DateExtra.toISOString event.from
                            ++ " to "
                            ++ DateExtra.toISOString event.to
                    ]
                ]

        list =
            if List.isEmpty model.events then
                [ li [ class "list-group-item" ]
                    [ div [ class "text-muted text-xs-center" ] [ text "no events yet" ]
                    ]
                ]
            else
                List.map makeEvent model.events
    in
        [ div [ class "card" ]
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
                * (model.now - Date.toTime model.birthDate)
                / (Date.toTime (deathDate model) - Date.toTime model.birthDate)
                |> clamp 0 100
                |> roundToPadded 6
    in
        [ div [ class "row mt-2" ]
            [ div [ class "col-xs-6" ]
                [ h4 [] [ span [ class "tag tag-default" ] [ text <| percentage ++ "% elapsed" ] ]
                ]
            , div [ class "col-xs-6 text-xs-right" ]
                [ h4 [] [ span [ class "tag tag-default" ] [ text <| left ++ " weeks left" ] ]
                ]
            ]
        , div [ class "row mt-1" ]
            [ div [ class "col-xs-12" ]
                [ progress
                    [ class "progress progress-striped"
                    , value percentage
                    , Html.Attributes.max "100"
                    , style [ ( "height", "1.7rem" ) ]
                    ]
                    []
                ]
            ]
        ]


footer : List (Html Msg)
footer =
    [ div [ class "row mt-1" ]
        [ div [ class "col-xs-12" ]
            [ div [ class "alert alert-info" ]
                [ strong [] [ text "Privacy: " ]
                , text "this page runs entirely in your browser so no data is sent to any server."
                ]
            , p [ class "text-xs-center" ]
                [ text "Inspired by "
                , a [ href "https://i.imgur.com/67aHKhF.jpg", target "_blank" ] [ text "this chart" ]
                , text " - "
                , a [ href "https://www.reddit.com/r/GetMotivated/comments/1vyf9r/made_for_myself_thought_of_you_weeks_left/cexas8u/", target "_blank" ] [ text "I'm terrified" ]
                , text " - "
                , a [ href "https://github.com/infertux/lifechart", target "_blank" ] [ text "Source code" ]
                , text " - "
                , a [ href "javascript:void(0)", onClick ToggleModal ] [ text "Show raw data" ]
                ]
            , div [ class "hidden-xs-up" ]
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
            Date.toTime (deathDate model) - model.now

        weeks =
            (Time.inHours left) / 24 / 7 |> Basics.max 0
    in
        roundToPadded 6 weeks
