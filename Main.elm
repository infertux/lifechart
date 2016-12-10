module LifeChart exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)
import Date exposing (..)
import String


---- MODEL ----


type alias Model =
    { zoomLevel : ZoomLevel
    , squares : List Square
    , lifeExpectancy : Float
    , birthDate : Date
    , hideUnproductiveYears :
        Bool
        -- https://www.reddit.com/r/GetMotivated/comments/1vyf9r/made_for_myself_thought_of_you_weeks_left/cexas8u
    }


type alias Square =
    { number : Int
    , activities : List Activity
    , countries : List Country
    }


type alias Country =
    { code : String
    , name :
        String
        --, flag : TODO?
    }


type alias Event =
    { label : String
    , date : Date
    }


type alias Goal =
    { label : String
    , date :
        Date
        --, recurring : TODO?
    }


type alias Activity =
    { label : String
    , color : Color
    }


type alias Item =
    { kind : ItemKind
    , from : Date
    , to : Date
    }


type ItemKind
    = ActivityKind
    | CountryKind
    | EventKind
    | GoalKind


type
    ZoomLevel
    -- size of 1 square
    = Day
    | Week
    | Month



-- dummyItems =
--   [ Item (Activity "Job" (rgb 100 200 200)) (Date.fromTime 0) (Date.fromTime 10000)
--   ]


dummyActivities =
    [ Activity "Baby" (rgb 50 100 100)
    , Activity "Lower School" (rgb 50 100 150)
    , Activity "Upper School" (rgb 100 100 200)
    , Activity "Job" (rgb 100 200 200)
    , Activity "Retirement" (rgb 100 220 100)
    , Activity "N/A" (rgb 255 255 255)
    ]


dummyCountries =
    [ Country "FR" "France"
    , Country "DE" "Germany"
    ]


newSquare : Int -> Square
newSquare number =
    Square number (List.take 2 dummyActivities) dummyCountries


initialModel : Model
initialModel =
    { zoomLevel = Month
    , squares = List.map (\i -> newSquare i) (List.range 0 (12 * 50 - 1))
    , lifeExpectancy = 80
    , birthDate = dummyBirthDate
    , hideUnproductiveYears = True
    }


dummyBirthDate =
    Date.fromTime (647948800 * 1000)


squarePerRow =
    12



-- merge squares when attributes are identical?


squareColor : Square -> Color
squareColor square =
    List.head (List.map .color square.activities)
        -- do a OR of colors instead of taking head?
        |>
            Maybe.withDefault (rgb 255 255 255)



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


squareYear : Square -> Int
squareYear square =
    --(square.startDate - model.birthDate) / model.zoomLevel -- TODO
    square.number // 12


dateToISO : Date -> String
dateToISO date =
    List.foldr (++)
        ""
        [ date |> Date.year |> toString
        , "-"
        , date |> Date.month |> toString
        , "-"
        , date |> Date.day |> toString
        ]



---- UPDATE ----


type Msg
    = NoOp
    | UpdateLifeExpectancy Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateLifeExpectancy years ->
            ( { model | lifeExpectancy = years }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ class "lifechart-wrapper"
        ]
        [ section [] [ text (dateToISO model.birthDate) ]
        , section
            [ id "lifechart" ]
            [ squareList model.squares
              -- , lazy3 controls model.visibility model.tasks
            ]
        , infoFooter
        ]


squareDivSize =
    if squarePerRow >= 52 then
        20
    else
        80


squareDiv : Square -> Html Msg
squareDiv square =
    div
        [ class "square"
        , style
            [ ( "display", "inline-block" )
            , ( "width", toString squareDivSize ++ "px" )
            , ( "height", toString squareDivSize ++ "px" )
            , ( "border", "1px dotted #555" )
            , ( "overflow", "hidden" )
            , ( "font-size", "8px" )
            , ( "background-color", (colorToCss (squareColor square)) )
            ]
        ]
        (List.map countryDiv square.countries)


countryDiv : Country -> Html Msg
countryDiv country =
    div [ style [ ( "float", "right" ) ] ] [ text country.code ]


squareList : List Square -> Html Msg
squareList squares =
    let
        el square =
            if square.number % squarePerRow == 0 then
                [ div [] []
                , div
                    [ style [ ( "display", "inline-block" ), ( "width", toString squareDivSize ++ "px" ) ] ]
                    [ text (toString (squareYear square)) ]
                , squareDiv square
                ]
            else
                [ squareDiv square ]
    in
        div [] (List.concat (List.map el squares))


infoFooter : Html Msg
infoFooter =
    footer [ id "info" ]
        [ p [] [ text "Double-click to edit a square" ]
        , p []
            [ text "Inspired by "
            , a [ href "http://i.imgur.com/67aHKhF.jpg" ] [ text "this chart" ]
            ]
        ]


colorToCss : Color -> String
colorToCss color =
    let
        r =
            toRgb color

        rgb =
            (List.intersperse "," (List.map toString [ r.red, r.green, r.blue ]))
    in
        "rgb(" ++ String.concat rgb ++ ")"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
