module Lifechart.Model exposing (..)

import Date exposing (Date)
import Color exposing (Color)
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
    , hideUnproductiveYears : Bool
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
    , hideUnproductiveYears : Bool
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
    , hideUnproductiveYears = False
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
        , hideUnproductiveYears = jsonModel.hideUnproductiveYears
        , events = jsonModel.events
    }


mergeModel : Model -> Model -> Model
mergeModel newModel baseModel =
    { newModel
        | eventFormOpen = baseModel.eventFormOpen
        , eventForm = baseModel.eventForm
        , modalOpen = baseModel.modalOpen
        , now = baseModel.now
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
    | HideUnproductiveYears Bool
    | ShowEventForm Int
    | UpdateEvent NewEventField String
    | UpdateEventOverlay Bool
    | SaveEvent
    | DeleteEvent
    | NewConfig String
    | ToggleModal


relativeBirthDate : Model -> Date
relativeBirthDate model =
    if model.hideUnproductiveYears then
        partialDate model (Date.year model.birthDate + model.kidUntil)
    else
        model.birthDate


relativeDeathDate : Model -> Date
relativeDeathDate model =
    if model.hideUnproductiveYears then
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
