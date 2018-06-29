module Lifechart.Model exposing (..)

import Date exposing (Date)
import Color exposing (Color)
import Color.Convert
import Time exposing (Time)
import Navigation
import DateExtra


type alias Model =
    { birthDate : Date
    , birthDateString : String
    , kidUntil : Int
    , oldFrom : Int
    , lifeExpectancy : Int
    , lifeExpectancyString : String
    , onlyAdultYears : Bool
    , events : List Event
    , eventFormOpen : Int
    , eventForm : EventForm
    , modalOpen : Bool
    , now : Time
    }


type alias JsonModel =
    { birthDate : Date
    , kidUntil : Int
    , oldFrom : Int
    , lifeExpectancy : Int
    , onlyAdultYears : Bool
    , events : List Event
    }


type alias Event =
    { from : Date
    , to : Date
    , color : Color
    , label : String
    , overlay : Bool
    }


type alias EventForm =
    { from : String
    , to : String
    , color : String
    , label : String
    , overlay : Bool
    }


initialModel : Model
initialModel =
    { birthDate = DateExtra.unsafeFromString "1988-07-24"
    , birthDateString = "1988-07-24"
    , kidUntil = 18
    , oldFrom = 70
    , lifeExpectancy = 80
    , lifeExpectancyString = "80"
    , onlyAdultYears = False
    , events = []
    , eventFormOpen = -1
    , eventForm = EventForm "" "" "" "" False
    , modalOpen = False
    , now = 0
    }


mergeJsonModel : JsonModel -> Model
mergeJsonModel jsonModel =
    { initialModel
        | birthDate = jsonModel.birthDate
        , birthDateString = DateExtra.toISOString jsonModel.birthDate
        , kidUntil = jsonModel.kidUntil
        , oldFrom = jsonModel.oldFrom
        , lifeExpectancy = jsonModel.lifeExpectancy
        , lifeExpectancyString = toString jsonModel.lifeExpectancy
        , onlyAdultYears = jsonModel.onlyAdultYears
        , events = jsonModel.events
    }


mergeModel : Model -> Model -> Model
mergeModel baseModel newModel =
    { newModel
        | eventFormOpen = baseModel.eventFormOpen
        , eventForm = baseModel.eventForm
        , modalOpen = baseModel.modalOpen
        , now = baseModel.now
    }


initialEventForm : Model -> EventForm
initialEventForm model =
    { from = DateExtra.toISOString <| Date.fromTime model.now
    , to = DateExtra.toISOString <| Date.fromTime model.now
    , label = ""
    , overlay = False
    , color = Color.Convert.colorToHex fallbackColor
    }


createEvent : EventForm -> Event
createEvent form =
    { from = DateExtra.fromStringWithFallback form.from (Date.fromTime 0)
    , to = DateExtra.fromStringWithFallback form.to (Date.fromTime 0)
    , label = form.label
    , overlay = form.overlay
    , color =
        Color.Convert.hexToColor form.color
            |> Result.toMaybe
            |> Maybe.withDefault fallbackColor
    }


fallbackColor : Color
fallbackColor =
    Color.red


type NewEventField
    = EventFrom
    | EventTo
    | EventColor
    | EventLabel


type Msg
    = Tick Time
    | NewUrl Navigation.Location
    | NewBirthDate String
    | NewLifeExpectancy String
    | OnlyAdultYears Bool
    | ShowEventForm Int
    | UpdateEvent NewEventField String
    | UpdateEventOverlay Bool
    | SaveEvent
    | DeleteEvent
    | NewConfig String
    | ToggleModal


relativeBirthDate : Model -> Date
relativeBirthDate model =
    if model.onlyAdultYears then
        partialDate model (Date.year model.birthDate + model.kidUntil)
    else
        model.birthDate


relativeDeathDate : Model -> Date
relativeDeathDate model =
    if model.onlyAdultYears then
        partialDate model (Date.year model.birthDate + maxOldFrom model)
    else
        partialDate model (Date.year model.birthDate + model.lifeExpectancy)


maxKidUntil : Model -> Int
maxKidUntil model =
    Basics.min model.lifeExpectancy model.kidUntil


maxOldFrom : Model -> Int
maxOldFrom model =
    Basics.min model.lifeExpectancy model.oldFrom


partialDate : Model -> Int -> Date
partialDate model year =
    DateExtra.unsafeFromString <|
        String.join "-"
            [ toString year
            , toString <| DateExtra.monthToInt <| Date.month model.birthDate
            , toString <| Date.day model.birthDate
            ]


eventInitial : Event -> String
eventInitial event =
    event.label |> String.left 1 |> String.toUpper


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
