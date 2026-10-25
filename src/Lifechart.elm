module Lifechart exposing (main)

import Browser
import Time
import Lifechart.Model as Model
import Lifechart.Update exposing (init, update)
import Lifechart.View exposing (view)


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
    -- every 10 seconds (Time.every takes milliseconds)
    Time.every (10 * 1000) Model.Tick
