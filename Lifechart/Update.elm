module Lifechart.Update exposing (init, update)

import Navigation
import Date
import DateExtra
import Time
import Task
import Regex
import Color.Convert
import Lifechart.Model exposing (..)
import Lifechart.Serializer as Serializer


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            decodeModel location |> Result.toMaybe |> Maybe.withDefault initialModel
    in
        ( model, Task.perform Tick Time.now )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | now = time }, Cmd.none )

        NewUrl location ->
            let
                newModel =
                    case decodeModel location of
                        Ok urlModel ->
                            mergeModel model urlModel

                        Err _ ->
                            model
            in
                ( newModel, Cmd.none )

        NewBirthDate string ->
            let
                stringModel =
                    { model | birthDateString = string }

                validFormat =
                    Regex.contains (Regex.regex "^\\d{4}-\\d{2}-\\d{2}$") string

                -- XXX: we need this pre-check for browsers without support for
                -- input[type='date'] which may send garbage as the date string
                date =
                    if validFormat then
                        Date.fromString string
                    else
                        Err ""
            in
                case date of
                    Ok date ->
                        let
                            newModel =
                                { stringModel | birthDate = date }
                        in
                            ( newModel, updateUrl newModel )

                    Err _ ->
                        ( stringModel, Cmd.none )

        NewLifeExpectancy string ->
            let
                stringModel =
                    { model | lifeExpectancyString = string }

                int =
                    String.toInt string |> Result.toMaybe |> Maybe.withDefault 0
            in
                if int < minLifeExpectancy model || int > 500 then
                    ( stringModel, Cmd.none )
                else
                    let
                        newModel =
                            { stringModel | lifeExpectancy = int }
                    in
                        ( newModel, updateUrl newModel )

        HideUnproductiveYears bool ->
            let
                -- TODO: fix this dirty hack somehow
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
                            initialEventForm model

                        Just event ->
                            { from = DateExtra.toISOString event.from
                            , to = DateExtra.toISOString event.to
                            , color = Color.Convert.colorToHex event.color
                            , label = event.label
                            , overlay = event.overlay
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

        UpdateEventOverlay bool ->
            let
                form =
                    model.eventForm

                newForm =
                    { form | overlay = bool }
            in
                ( { model | eventForm = newForm }, Cmd.none )

        SaveEvent ->
            let
                existingEvent =
                    model.eventFormOpen /= 0

                currentEvent =
                    createEvent model.eventForm

                otherEvents =
                    if existingEvent then
                        deleteItem (model.eventFormOpen - 1) model.events
                    else
                        model.events

                events =
                    (currentEvent :: otherEvents)
                        |> List.sortBy (\event -> DateExtra.toISOString event.from)

                newModel =
                    { model | events = events, eventFormOpen = -1 }
            in
                ( newModel, updateUrl newModel )

        DeleteEvent ->
            let
                events =
                    deleteItem (model.eventFormOpen - 1) model.events

                newModel =
                    { model | events = events, eventFormOpen = -1 }
            in
                ( newModel, updateUrl newModel )

        NewConfig json ->
            let
                newModel =
                    case Serializer.deserializeJson json of
                        Ok jsonModel ->
                            mergeModel model jsonModel

                        Err _ ->
                            model
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


deleteItem : Int -> List a -> List a
deleteItem index list =
    List.indexedMap
        (\i ->
            \item ->
                if i == index then
                    Nothing
                else
                    Just item
        )
        list
        |> List.filterMap identity
