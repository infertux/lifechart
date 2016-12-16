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

        NewBirthDate string ->
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

        ShowEventForm id ->
            let
                newId =
                    if model.eventFormOpen == id then
                        -1
                    else
                        id

                event =
                    List.take newId model.events |> List.reverse |> List.head

                eventForm =
                    case event of
                        Nothing ->
                            { from = DateExtra.toISOString <| Date.fromTime model.now
                            , to = DateExtra.toISOString <| Date.fromTime model.now
                            , color = Color.Convert.colorToHex fallbackColor
                            , label = ""
                            , location = False
                            }

                        Just event ->
                            { from = DateExtra.toISOString event.from
                            , to = DateExtra.toISOString event.to
                            , color = Color.Convert.colorToHex event.color
                            , label = event.label
                            , location = event.location
                            }
            in
                ( { model | eventFormOpen = newId, eventForm = eventForm }, Cmd.none )

        UpdateEvent field value ->
            let
                form =
                    model.eventForm

                -- TODO: this is kinda gross, is there a better way?
                newForm =
                    case field of
                        EventFrom ->
                            { form | from = value }

                        EventTo ->
                            { form | to = value }

                        EventColor ->
                            { form | color = value }

                        EventLabel ->
                            { form | label = value }
            in
                ( { model | eventForm = newForm }, Cmd.none )

        UpdateEventLocation bool ->
            let
                form =
                    model.eventForm

                newForm =
                    { form | location = bool }
            in
                ( { model | eventForm = newForm }, Cmd.none )

        SaveEvent ->
            let
                eventForm =
                    model.eventForm

                newEvent =
                    { from =
                        DateExtra.fromStringWithFallback eventForm.from (Date.fromTime 0)
                    , to =
                        DateExtra.fromStringWithFallback eventForm.to (Date.fromTime 0)
                    , color =
                        Color.Convert.hexToColor eventForm.color |> Maybe.withDefault fallbackColor
                    , label = eventForm.label
                    , location = eventForm.location
                    }

                newEvents =
                    if model.eventFormOpen == 0 then
                        model.events
                    else
                        deleteEvent model.eventFormOpen model.events

                events =
                    (newEvent :: newEvents)
                        |> List.sortBy (\event -> DateExtra.toISOString event.from)

                newModel =
                    { model | events = events, eventFormOpen = -1 }
            in
                ( newModel, updateUrl newModel )

        DeleteEvent ->
            let
                events =
                    deleteEvent model.eventFormOpen model.events

                newModel =
                    { model | events = events, eventFormOpen = -1 }
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


deleteEvent : Int -> List Event -> List Event
deleteEvent index events =
    List.indexedMap
        (\i ->
            \event ->
                if i + 1 == index then
                    Nothing
                else
                    Just event
        )
        events
        |> List.filterMap identity
