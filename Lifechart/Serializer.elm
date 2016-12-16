module Lifechart.Serializer
    exposing
        ( serialize
        , serializeJson
        , deserialize
        , deserializeJson
        )

import Color exposing (Color)
import Color.Convert
import Date exposing (Date)
import DateExtra
import Json.Decode as Decode
import Json.Encode as Encode
import Base64
import Lifechart.Model exposing (..)


serialize : Model -> String
serialize model =
    case Base64.encode (serializeJson model) of
        Err _ ->
            ""

        Ok string ->
            string


deserialize : String -> Result String Model
deserialize base64 =
    case Base64.decode base64 of
        Err error ->
            Err error

        Ok json ->
            deserializeJson json


serializeJson : Model -> String
serializeJson model =
    Encode.encode 2 <|
        Encode.object
            [ ( "birth-date", encodeDate model.birthDate )
            , ( "life-expectancy", Encode.int model.lifeExpectancy )
            , ( "kid-until", Encode.int model.kidUntil )
            , ( "old-from", Encode.int model.oldFrom )
            , ( "hide-unproductive-years", Encode.bool model.hideUnproductiveYears )
            , ( "events", Encode.list <| List.map encodeEvent model.events )
            ]


jsonDecoder : Decode.Decoder JsonModel
jsonDecoder =
    Decode.map6 JsonModel
        (Decode.field "birth-date" dateDecoder)
        (Decode.field "kid-until" Decode.int)
        (Decode.field "old-from" Decode.int)
        (Decode.field "life-expectancy" Decode.int)
        (Decode.field "hide-unproductive-years" Decode.bool)
        (Decode.field "events" <| Decode.list eventDecoder)


deserializeJson : String -> Result String Model
deserializeJson json =
    case
        Decode.decodeString jsonDecoder json
    of
        Err error ->
            Err error

        Ok jsonModel ->
            Ok (mergeJsonModel jsonModel)


encodeEvent : Event -> Encode.Value
encodeEvent event =
    Encode.object
        [ ( "from", encodeDate event.from )
        , ( "to", encodeDate event.to )
        , ( "color", encodeColor event.color )
        , ( "label", Encode.string event.label )
        , ( "location", Encode.bool event.location )
        ]


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.map5 Event
        (Decode.field "from" dateDecoder)
        (Decode.field "to" dateDecoder)
        (Decode.field "color" colorDecoder)
        (Decode.field "label" Decode.string)
        (Decode.field "location" Decode.bool)


encodeDate : Date -> Encode.Value
encodeDate date =
    Encode.string <| DateExtra.toISOString date


dateDecoder : Decode.Decoder Date.Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case Date.fromString string of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        Decode.fail error
            )


encodeColor : Color -> Encode.Value
encodeColor color =
    Encode.string <| Color.Convert.colorToHex color


colorDecoder : Decode.Decoder Color
colorDecoder =
    Decode.string
        |> Decode.andThen
            (\hex ->
                Color.Convert.hexToColor hex
                    |> Maybe.withDefault fallbackColor
                    |> Decode.succeed
            )
