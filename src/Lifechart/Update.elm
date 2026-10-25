module Lifechart.Update exposing (init, update)

import Browser
import Browser.Navigation as Navigation
import Date
import Time
import Task
import Regex
import ColorExtra
import Url exposing (Url)
import Lifechart.Model exposing (..)
import Lifechart.Serializer as Serializer


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        baseModel =
            initialModel key

        model =
            decodeModel baseModel url |> Result.withDefault baseModel
    in
        ( model, Task.perform Tick Time.now )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | now = time }, Cmd.none )

        NewUrl url ->
            ( decodeModel model url |> Result.withDefault model, Cmd.none )

        UrlRequest (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        UrlRequest (Browser.External href) ->
            ( model, Navigation.load href )

        NewBirthDate string ->
            let
                stringModel =
                    { model | birthDateString = string }

                validFormat =
                    Regex.fromString "^\\d{4}-\\d{2}-\\d{2}$"
                        |> Maybe.map (\regex -> Regex.contains regex string)
                        |> Maybe.withDefault False

                -- XXX: we need this pre-check for browsers without support for
                -- input[type='date'] which may send garbage as the date string
                date =
                    if validFormat then
                        Date.fromIsoString string
                    else
                        Err ""
            in
                case date of
                    Ok birthDate ->
                        let
                            newModel =
                                { stringModel | birthDate = birthDate }
                        in
                            ( newModel, updateUrl newModel )

                    Err _ ->
                        ( stringModel, Cmd.none )

        NewLifeExpectancy string ->
            let
                stringModel =
                    { model | lifeExpectancyString = string }

                int =
                    String.toInt string |> Maybe.withDefault 0
            in
                if int < minLifeExpectancy model || int > 500 then
                    ( stringModel, Cmd.none )
                else
                    let
                        newModel =
                            { stringModel | lifeExpectancy = int }
                    in
                        ( newModel, updateUrl newModel )

        OnlyAdultYears bool ->
            let
                -- TODO: fix this dirty hack somehow
                tempModel =
                    { model | onlyAdultYears = bool }

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

                        Just existingEvent ->
                            { from = Date.toIsoString existingEvent.from
                            , to = Date.toIsoString existingEvent.to
                            , color = ColorExtra.toHex existingEvent.color
                            , label = existingEvent.label
                            , overlay = existingEvent.overlay
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
                        |> List.sortBy (\event -> Date.toRataDie event.from)

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
                            mergeJsonModel model jsonModel

                        Err _ ->
                            model
            in
                ( newModel, updateUrl newModel )

        ToggleModal ->
            ( { model | modalOpen = not model.modalOpen }, Cmd.none )


decodeModel : Model -> Url -> Result String Model
decodeModel model url =
    let
        dataWithoutHash =
            Maybe.withDefault "" url.fragment
    in
        if String.isEmpty dataWithoutHash then
            Ok (mergeModel model (initialModel model.key))
        else
            Serializer.deserialize dataWithoutHash
                |> Result.map (mergeJsonModel model)


updateUrl : Model -> Cmd Msg
updateUrl model =
    let
        base64 =
            Serializer.serialize model

        url =
            "#" ++ base64
    in
        Navigation.pushUrl model.key url


minLifeExpectancy : Model -> Int
minLifeExpectancy model =
    if model.onlyAdultYears then
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
