module LifeChart where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- import Html.Lazy exposing (lazy, lazy2, lazy3)
import Color exposing (..)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import Date exposing (..)
import String
import Window


---- MODEL ----

type alias Model =
  { weeks : List Week
  , lifeExpectancy : Float
  , birthDate : Date
  }

type alias Week =
  { number : Int
  , activities : List Activity
  , countries : List Country
  }

type alias Country =
  { code : String
  , name : String
  --, flag : TODO?
  }

type alias Event =
  { label : String
  , date : Date
  }

type alias Goal =
  { label : String
  , date : Date
  --, recurring : TODO?
  }

type alias Activity =
  { label : String
  , color : Color
  }

dummyActivities =
  [ Activity "Baby"         (rgb  50 100 100)
  , Activity "Lower School" (rgb  50 100 150)
  , Activity "Upper School" (rgb 100 100 200)
  , Activity "Job"          (rgb 100 200 200)
  , Activity "Retirement"   (rgb 100 220 100)
  , Activity "N/A"          (rgb 255 255 255)
  ]

dummyCountries =
  [ Country "FR" "France"
  , Country "DE" "Germany"
  ]

newWeek : Int -> Week
newWeek number =
  Week number (List.take 2 dummyActivities) dummyCountries

dummyModel : Model
dummyModel =
    { weeks = List.map (\i -> newWeek i) [0..52*50-1]
    , lifeExpectancy = 80
    , birthDate = dummyBirthDate
    }

dummyBirthDate = Date.fromTime (647948800*1000)
weekPerRow = 52

weekColor : Week -> Color
weekColor week =
  List.head (List.map .color week.activities) -- do a OR of colors instead of taking head?
  |> Maybe.withDefault (rgb 255 255 255)
  -- if number < (Date.day dummyBirthDate) // 7 then -- FIXME
  --   rgb 255 255 255
  -- else if number < 52*4-4*4 then
  --   rgb 50 100 100
  -- else if number < 52*18-4*4 then
  --   rgb 50 100 150
  -- else if number < 52*24-4*9 then
  --   rgb 100 100 200
  -- else if number < 52*26-6 then
  --   rgb 100 200 200
  -- else
  --   rgb 100 220 100

dateToISO : Date -> String
dateToISO date =
  List.foldr (++) ""
    [ date |> Date.year |> toString
    , "-"
    , date |> Date.month |> toString
    , "-"
    , date |> Date.day |> toString
    ]

---- UPDATE ----

type Action
    = NoOp
    | UpdateLifeExpectancy Float

update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      UpdateLifeExpectancy years ->
          { model | lifeExpectancy <- years }


---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    div
      [ class "lifechart-wrapper"
      ]
      [ section [] [ text (dateToISO model.birthDate) ]
      , section
          [ id "lifechart" ]
          [ weekList address model.weeks
          -- , lazy3 controls address model.visibility model.tasks
          ]
      , infoFooter
      ]

weekDivSize = if weekPerRow >= 52 then 20 else 200
weekDiv : Week -> Html
weekDiv week =
  div
  [ class "week", style [
    ("display", "inline-block")
  , ("width", toString weekDivSize ++ "px")
  , ("height", toString weekDivSize ++ "px")
  , ("border", "1px solid #555")
  , ("overflow", "hidden")
  , ("font-size", "6px")
  , ("background-color", (colorToCss (weekColor week))) ] ]
  (List.map countryDiv week.countries)

countryDiv : Country -> Html
countryDiv country =
  div [ style [ ("float", "right") ] ] [ text country.code ]

weekList : Address Action -> List Week -> Html
weekList address weeks =
  let el week =
    if week.number % weekPerRow == 0 then
      [ div [] []
      , div
        [ style [ ("display", "inline-block"), ("width", toString weekDivSize ++ "px") ] ]
        [ text (toString (floor (toFloat week.number/52) + 1)) ]
      , weekDiv week
      ]
    else
      [weekDiv week]
  in
    div [] (List.concat (List.map el weeks))

infoFooter : Html
infoFooter =
    footer [ id "info" ]
      [ p [] [ text "Double-click to edit a week" ]
      , p []
          [ text "Inspired by "
          , a [ href "http://i.imgur.com/67aHKhF.jpg" ] [ text "this chart" ]
          ]
      ]

colorToCss : Color -> String
colorToCss color =
  let
    r = toRgb color
    rgb = (List.intersperse "," (List.map toString [r.red, r.green, r.blue]))
  in
    "rgb(" ++ String.concat rgb ++ ")"

---- INPUTS ----

-- wire the entire application together
main : Signal Html
main =
  Signal.map (view actions.address) model


-- manage the model of our application over time
model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


initialModel : Model
initialModel =
  --Maybe.withDefault dummyModel getStorage
  dummyModel


-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


-- port focus : Signal String
-- port focus =
--     let needsFocus act =
--             case act of
--               EditingTask id bool -> bool
--               _ -> False

--         toSelector (EditingTask id _) = ("#todo-" ++ toString id)
--     in
--         actions.signal
--           |> Signal.filter needsFocus (EditingTask 0 True)
--           |> Signal.map toSelector


-- interactions with localStorage to save the model
--port getStorage : Maybe Model

--port setStorage : Signal Model
--port setStorage = model
