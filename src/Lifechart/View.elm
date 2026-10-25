module Lifechart.View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
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
                    [ div [ class "col-6" ] [ Controls.controls model ]
                    , div [ class "col-6" ]
                        [ if Time.posixToMillis model.now == 0 then
                            h1 [ class "text-center" ] [ text "Loading..." ]

                          else
                            Canvas.canvas model
                        ]
                    ]
                ]
            ]
        ]
    }
