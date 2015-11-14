module LifeChart where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- import Html.Lazy exposing (lazy, lazy2, lazy3)
import Color exposing (..)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Window


---- MODEL ----

type alias Model =
    { weeks : List Week
    , lifeExpectancy : Float
    }

type alias Week =
    { number : Int
    , color : Color
    }

-- newTask : String -> Int -> Task
-- newTask desc id =
--     { description = desc
--     , completed = False
--     , editing = False
--     , id = id
--     }

emptyModel : Model
emptyModel =
    { weeks = List.map (\i -> Week i (weekColor i)) [1..700]
    , lifeExpectancy = 80
    }

weekColor : Int -> Color
weekColor number =
  (rgb ((number%weekPerRow)*round (255/weekPerRow)) 100 10)

weekPerRow = 52

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
      [ section
          [ id "lifechart" ]
          [ weekList address model.weeks
          -- , lazy3 controls address model.visibility model.tasks
          ]
      , infoFooter
      ]

-- onEnter : Address a -> a -> Attribute
-- onEnter address value =
--     on "keydown"
--       (Json.customDecoder keyCode is13)
--       (\_ -> Signal.message address value)

-- is13 : Int -> Result String ()
-- is13 code =
--   if code == 13 then Ok () else Err "not the right key code"

weekDiv : Week -> Html
weekDiv week =
  div
  [ class "week", style [
    ("display", "inline-block")
  , ("width", "20px")
  , ("height", "20px")
  , ("border", "1px solid #ccc")
  , ("background-color", (colorToCss week.color)) ] ]
  [ text (toString (week.number%weekPerRow)) ]

weekList : Address Action -> List Week -> Html
weekList address weeks =
  let el week =
    if week.number % weekPerRow == 0 then
      [weekDiv week, div [] []]
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
  --Maybe.withDefault emptyModel getStorage
  emptyModel


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
