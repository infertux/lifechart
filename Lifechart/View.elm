module Lifechart.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Lifechart.Model exposing (..)
import Lifechart.View.Canvas as Canvas
import Lifechart.View.Controls as Controls


view : Model -> Html Msg
view model =
    if model.now == 0 then
        h1 [ class "text-xs-center" ] [ text "Loading..." ]
    else
        div []
            [ Controls.modal model
            , div [ class "row" ]
                [ div [ class "col-xs-6" ] [ Controls.controls model ]
                , div [ class "col-xs-6" ] [ Canvas.canvas model ]
                ]
            ]
