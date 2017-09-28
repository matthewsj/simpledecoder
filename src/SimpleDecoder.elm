module SimpleDecoder exposing (JsonValue, toJsonValue, wrapPort)

{-| Library for writing simple JSON decoders.

@docs JsonValue, toJsonValue, wrapPort

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import OneStepDecoder


{-| Core JSON type.
-}
type JsonValue
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Null
    | Obj (Dict String JsonValue)
    | Arr (List JsonValue)


{-| Gets a JsonValue from a Value.
-}
readJsonValue : Decode.Value -> JsonValue
readJsonValue v =
    case Decode.decodeValue toJsonValue v of
        Ok jsonValue ->
            jsonValue

        Err s ->
            Debug.crash ("Error reading JSON value generically: " ++ s)


{-| Decoder that turns JSON into JsonValue objects that can be further parsed by client code.
-}
toJsonValue : Decoder JsonValue
toJsonValue =
    OneStepDecoder.toOneStepValue
        |> Decode.map
            (\osv ->
                case osv of
                    OneStepDecoder.String s ->
                        String s

                    OneStepDecoder.Int i ->
                        Int i

                    OneStepDecoder.Float f ->
                        Float f

                    OneStepDecoder.Bool b ->
                        Bool b

                    OneStepDecoder.Null ->
                        Null

                    OneStepDecoder.Obj dict ->
                        Obj (Dict.map (\_ v -> readJsonValue v) dict)

                    OneStepDecoder.Arr list ->
                        Arr (List.map readJsonValue list)
            )



toJsonString : Decoder JsonValue
toJsonString =
    Decode.map String Decode.string


toJsonNumber : Decoder JsonValue
toJsonNumber =
    Decode.oneOf
        [ Decode.map Float Decode.float
        , Decode.map Int Decode.int
        ]


toJsonObject : Decoder JsonValue
toJsonObject =
    Decode.map Obj (Decode.lazy (\_ -> Decode.dict toJsonValue))


toJsonArray : Decoder JsonValue
toJsonArray =
    Decode.map Arr (Decode.lazy (\_ -> Decode.list toJsonValue))


toJsonBool : Decoder JsonValue
toJsonBool =
    Decode.map Bool Decode.bool


toJsonNull : Decoder JsonValue
toJsonNull =
    Decode.null Null


{-| Converts a Json.Decode.Value port into a JsonValue port.
-}
wrapPort : ((Decode.Value -> msg) -> Sub msg) -> (JsonValue -> msg) -> Sub msg
wrapPort p simpleDecoder =
    p (readJsonValue >> simpleDecoder)
