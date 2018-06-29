module Lifechart.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Lifechart.Model exposing (..)
import Lifechart.View.Canvas as Canvas
import Lifechart.View.Controls as Controls


view : Model -> Html Msg
view model =
    div []
        [ Controls.modal model
        , div [ class "row" ]
            [ div [ class "col-6" ] [ Controls.controls model ]
            , div [ class "col-6" ]
                [ if model.now == 0 then
                    h1 [ class "text-center" ] [ text "Loading..." ]
                  else
                    Canvas.canvas model
                ]
            ]
        ]
