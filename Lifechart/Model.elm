module Lifechart.Model exposing (..)

import Date exposing (Date)
import Color exposing (Color)
import Time exposing (Time)
import Navigation
import DateExtra


type alias Model =
    { dateOfBirth : Date
    , dateOfBirthString : String
    , kidUntil : Int
    , oldFrom : Int
    , lifeExpectancy : Int
    , events : List Event
    , newEventOpen : Bool
    , newEvent : Event
    , modalOpen : Bool
    , now : Time
    }


type alias JsonModel =
    { dateOfBirth : Date
    , kidUntil : Int
    , oldFrom : Int
    , lifeExpectancy : Int
    , events : List Event
    }


type alias Event =
    { from : Date
    , to : Date
    , color : Color
    , label : String
    }


initialModel : Model
initialModel =
    { dateOfBirth = DateExtra.unsafeFromString "1988-07-24"
    , dateOfBirthString = "1988-07-24"
    , kidUntil = 18
    , oldFrom = 70
    , lifeExpectancy = 80
    , events = []
    , newEventOpen = False
    , newEvent = initialEvent
    , modalOpen = False
    , now = 0
    }


mergeJsonModel : JsonModel -> Model
mergeJsonModel jsonModel =
    { initialModel
        | dateOfBirth = jsonModel.dateOfBirth
        , dateOfBirthString = DateExtra.toISOString jsonModel.dateOfBirth
        , kidUntil = jsonModel.kidUntil
        , oldFrom = jsonModel.oldFrom
        , lifeExpectancy = jsonModel.lifeExpectancy
        , events = jsonModel.events
    }


mergeModel : Model -> Model -> Model
mergeModel newModel baseModel =
    { newModel
        | newEventOpen = baseModel.newEventOpen
        , newEvent = baseModel.newEvent
        , modalOpen = baseModel.modalOpen
        , now = baseModel.now
    }


initialEvent : Event
initialEvent =
    { from = Date.fromTime 0
    , to = Date.fromTime 0
    , color = Color.black
    , label = ""
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
    | NewDateOfBirth String
    | NewLifeExpectancy String
    | ToggleNewEvent
    | UpdateNewEvent NewEventField String
    | SaveNewEvent
    | NewConfig String
    | ToggleModal


deathDate : Model -> Date
deathDate model =
    partialDate model (Date.year model.dateOfBirth + model.lifeExpectancy)


partialDate : Model -> Int -> Date
partialDate model year =
    DateExtra.unsafeFromString <|
        String.join "-"
            [ toString year
            , toString <| DateExtra.monthToInt <| Date.month model.dateOfBirth
            , toString <| Date.day model.dateOfBirth
            ]
