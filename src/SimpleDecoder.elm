module SimpleDecoder exposing (JsonValue(..), readJsonValue, toJsonValue, wrapPort, at)

{-| Library for writing simple JSON decoders.

@docs JsonValue, readJsonValue, toJsonValue, wrapPort, at

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


field : String -> JsonValue -> Result String JsonValue
field name jsonValue =
    case jsonValue of
        Obj dict ->
            case Dict.get name dict of
                Just fieldValue ->
                    Ok fieldValue

                Nothing ->
                    Err ("Field name " ++ name ++ " not found")

        _ ->
            Err "Value was not an object"


{-| Decodes a nested JSON object, following the given fields.
-}
at : List String -> JsonValue -> Result String JsonValue
at path v =
    case path of
        [] ->
            Ok v

        first :: rest ->
            case v of
                Obj d ->
                    case Dict.get first d of
                        Just subvalue ->
                            at rest subvalue

                        Nothing ->
                            Err ("Subpath " ++ first ++ " not found")

                _ ->
                    Err "Encountered a non-object"


type alias SDecoder t =
    JsonValue
    -> Result String t


orElse : (() -> Result x y) -> Result x y -> Result x y
orElse thunk result =
    case result of
        Ok _ ->
            result

        Err _ ->
            thunk ()


map : (s -> t) -> SDecoder s -> SDecoder t
map f decoder jsonValue =
    decoder jsonValue
        |> Result.map f


oneOf : List (SDecoder t) -> SDecoder t
oneOf decoders jsonValue =
    case decoders of
        [] ->
            Err "No decoder matched value"

        decoder :: rest ->
            decoder jsonValue |> orElse (\_ -> oneOf rest jsonValue)


andThen : (a -> SDecoder b) -> SDecoder a -> SDecoder b
andThen toB aDecoder jsonValue =
    case aDecoder jsonValue of
        Ok a ->
            (toB a) jsonValue

        Err s ->
            Err s
