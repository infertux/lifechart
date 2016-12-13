module Lifechart exposing (main)

import Navigation
import Time
import Lifechart.Model as Model
import Lifechart.Update exposing (init, update)
import Lifechart.View exposing (view)


main : Program Never Model.Model Model.Msg
main =
    Navigation.program Model.NewUrl
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Time.every Time.second Model.Tick
