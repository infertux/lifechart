module Lifechart.Update exposing (init, update)

import Navigation
import DateExtra
import Color.Convert
import Lifechart.Model exposing (..)
import Lifechart.Serializer as Serializer


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            decodeModel location
                |> Result.toMaybe
                |> Maybe.withDefault initialModel
    in
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | now = time }, Cmd.none )

        NewUrl location ->
            let
                urlModel =
                    decodeModel location
                        |> Result.toMaybe
                        |> Maybe.withDefault model

                newModel =
                    mergeModel urlModel model
            in
                ( newModel, Cmd.none )

        NewDateOfBirth string ->
            let
                date =
                    DateExtra.fromStringWithFallback string model.dateOfBirth

                newModel =
                    { model | dateOfBirth = date }
            in
                ( newModel, updateUrl newModel )

        NewLifeExpectancy string ->
            let
                value =
                    case String.toInt string of
                        Ok int ->
                            clamp 1 500 int

                        Err _ ->
                            1

                newModel =
                    { model | lifeExpectancy = value }
            in
                ( newModel, updateUrl newModel )

        ToggleNewEvent ->
            ( { model | newEventOpen = not model.newEventOpen }, Cmd.none )

        UpdateNewEvent field value ->
            let
                event =
                    model.newEvent

                -- TODO: this is kinda gross, is there a better way?
                newEvent =
                    case field of
                        EventFrom ->
                            { event | from = DateExtra.fromStringWithFallback value event.from }

                        EventTo ->
                            { event | to = DateExtra.fromStringWithFallback value event.to }

                        EventColor ->
                            { event | color = Color.Convert.hexToColor value |> Maybe.withDefault fallbackColor }

                        EventLabel ->
                            { event | label = value }
            in
                ( { model | newEvent = newEvent }, Cmd.none )

        SaveNewEvent ->
            let
                events =
                    List.append model.events [ model.newEvent ]

                newModel =
                    { model | events = events, newEventOpen = False }
            in
                ( newModel, updateUrl newModel )

        NewConfig json ->
            let
                jsonModel =
                    Serializer.deserializeJson json
                        |> Result.toMaybe
                        |> Maybe.withDefault model

                newModel =
                    mergeModel jsonModel model
            in
                ( newModel, updateUrl newModel )

        ToggleModal ->
            ( { model | modalOpen = not model.modalOpen }, Cmd.none )


decodeModel : Navigation.Location -> Result String Model
decodeModel location =
    let
        dataWithoutHash =
            String.dropLeft 1 location.hash
    in
        if String.isEmpty dataWithoutHash then
            Ok initialModel
        else
            Serializer.deserialize dataWithoutHash


updateUrl : Model -> Cmd Msg
updateUrl model =
    let
        base64 =
            Serializer.serialize model

        url =
            "#" ++ base64
    in
        Navigation.newUrl url
