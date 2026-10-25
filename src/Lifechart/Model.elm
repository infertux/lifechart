module Lifechart.Model exposing (..)

import Browser
import Browser.Navigation as Navigation
import Color exposing (Color)
import ColorExtra
import Date exposing (Date)
import DateExtra
import Time
import Url exposing (Url)


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
    , now : Time.Posix
    , key : Navigation.Key
    }


type alias JsonModel =
    { birthDate : Date
    , kidUntil : Int
    , oldFrom : Int
    , lifeExpectancy : Int
    , onlyAdultYears : Bool
    , events : List Event
    }


{-| The fields the chart is drawn from
-}
type alias Chart a =
    { a
        | birthDate : Date
        , kidUntil : Int
        , oldFrom : Int
        , lifeExpectancy : Int
        , onlyAdultYears : Bool
        , events : List Event
        , now : Time.Posix
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


initialModel : Navigation.Key -> Model
initialModel key =
    { birthDate = Date.fromCalendarDate 1988 Time.Jul 24
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
    , now = Time.millisToPosix 0
    , key = key
    }


mergeJsonModel : Model -> JsonModel -> Model
mergeJsonModel model jsonModel =
    { model
        | birthDate = jsonModel.birthDate
        , birthDateString = Date.toIsoString jsonModel.birthDate
        , kidUntil = jsonModel.kidUntil
        , oldFrom = jsonModel.oldFrom
        , lifeExpectancy = jsonModel.lifeExpectancy
        , lifeExpectancyString = String.fromInt jsonModel.lifeExpectancy
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
    { from = Date.toIsoString <| today model
    , to = Date.toIsoString <| today model
    , label = ""
    , overlay = False
    , color = ColorExtra.toHex fallbackColor
    }


createEvent : EventForm -> Event
createEvent form =
    { from = DateExtra.fromStringWithFallback form.from DateExtra.epoch
    , to = DateExtra.fromStringWithFallback form.to DateExtra.epoch
    , label = form.label
    , overlay = form.overlay
    , color =
        ColorExtra.fromHex form.color
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
    = Tick Time.Posix
    | NewUrl Url
    | UrlRequest Browser.UrlRequest
    | NewBirthDate String
    | NewLifeExpectancy String
    | OnlyAdultYears Bool
    | NewKidUntil String
    | NewOldFrom String
    | ShowEventForm Int
    | UpdateEvent NewEventField String
    | UpdateEventOverlay Bool
    | SaveEvent
    | DeleteEvent
    | NewConfig String
    | ToggleModal


today : Model -> Date
today model =
    Date.fromPosix Time.utc model.now


nowTime : Chart a -> Float
nowTime model =
    toFloat <| Time.posixToMillis model.now


relativeBirthDate : Chart a -> Date
relativeBirthDate model =
    if model.onlyAdultYears then
        partialDate model (Date.year model.birthDate + model.kidUntil)

    else
        model.birthDate


relativeDeathDate : Chart a -> Date
relativeDeathDate model =
    if model.onlyAdultYears then
        partialDate model (Date.year model.birthDate + maxOldFrom model)

    else
        partialDate model (Date.year model.birthDate + model.lifeExpectancy)


maxKidUntil : Chart a -> Int
maxKidUntil model =
    Basics.min model.lifeExpectancy model.kidUntil


maxOldFrom : Chart a -> Int
maxOldFrom model =
    Basics.min model.lifeExpectancy model.oldFrom


partialDate : Chart a -> Int -> Date
partialDate model year =
    Date.fromCalendarDate year (Date.month model.birthDate) (Date.day model.birthDate)


eventInitial : Event -> String
eventInitial event =
    event.label |> String.left 1 |> String.toUpper
