module Lifechart.View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy7)
import Lifechart.Model exposing (..)
import Lifechart.View.Canvas as Canvas
import Lifechart.View.Controls as Controls
import Time


view : Model -> Browser.Document Msg
view model =
    { title = "Lifechart"
    , body =
        [ div [ class "m-3" ]
            [ div [ class "container" ]
                [ Controls.modal model
                , div [ class "row" ]
                    [ div [ class "col-12 col-md-6" ]
                        [ Controls.controls model
                        , div [ class "d-none d-md-block" ] Controls.footer
                        ]
                    , div [ class "col-12 col-md-6 chart" ]
                        [ if Time.posixToMillis model.now == 0 then
                            h1 [ class "text-center" ] [ text "Loading..." ]

                          else
                            lazy7 Canvas.canvas
                                model.birthDate
                                model.kidUntil
                                model.oldFrom
                                model.lifeExpectancy
                                model.onlyAdultYears
                                model.events
                                (chartTime model)
                        ]

                    -- on phones the chart comes before the footer
                    , div [ class "col-12 d-md-none mt-3" ] Controls.footer
                    ]
                ]
            ]
        ]
    }


{-| The current time rounded down to the hour. Week squares only change at
midnight UTC and the current week moves at noon UTC, so this redraws the chart
once an hour instead of on every clock tick.
-}
chartTime : Model -> Int
chartTime model =
    let
        hour =
            60 * 60 * 1000
    in
    (Time.posixToMillis model.now // hour) * hour
