module Lifechart exposing (main)

import Browser
import Lifechart.Model as Model
import Lifechart.Update exposing (init, update)
import Lifechart.View exposing (view)
import Time


main : Program () Model.Model Model.Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = Model.UrlRequest
        , onUrlChange = Model.NewUrl
        }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    -- every second so that the weeks left tick like a clock (Time.every takes
    -- milliseconds)
    Time.every 1000 Model.Tick
