module Lifechart.Update exposing (init, update)

import Navigation
import Date
import DateExtra
import Time
import Task
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
        ( model, Task.perform Tick Time.now )


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
            case Date.fromString string of
                Ok date ->
                    let
                        newModel =
                            { model
                                | birthDateString = string
                                , birthDate = date
                            }
                    in
                        ( newModel, updateUrl newModel )

                Err _ ->
                    ( { model | birthDateString = string }, Cmd.none )

        NewLifeExpectancy string ->
            let
                int =
                    String.toInt string |> Result.toMaybe |> Maybe.withDefault 0
            in
                if int < minLifeExpectancy model || int > 500 then
                    ( { model | lifeExpectancyString = string }, Cmd.none )
                else
                    let
                        newModel =
                            { model
                                | lifeExpectancyString = string
                                , lifeExpectancy = int
                            }
                    in
                        ( newModel, updateUrl newModel )

        HideUnproductiveYears bool ->
            let
                tempModel =
                    { model | hideUnproductiveYears = bool }

                newModel =
                    { tempModel
                        | lifeExpectancy =
                            Basics.max tempModel.lifeExpectancy (minLifeExpectancy tempModel)
                    }
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
                    (model.newEvent :: model.events)
                        |> List.sortBy (\event -> DateExtra.toISOString event.from)

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


minLifeExpectancy : Model -> Int
minLifeExpectancy model =
    if model.hideUnproductiveYears then
        model.kidUntil
    else
        1
