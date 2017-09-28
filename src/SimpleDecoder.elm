module SimpleDecoder
    exposing
        ( JsonValue(..)
        , readJsonValue
        , toJsonValue
        , wrapPort
        , SDecoder
        , string
        , int
        , float
        , bool
        , null
        , field
        , at
        , map
        , oneOf
        , andThen
        )

{-| Library for writing simple JSON decoders.

@docs JsonValue, readJsonValue, toJsonValue, wrapPort

Functions and types for manipulating JsonValue values:
@docs SDecoder, string, int, float, bool, null, field, at, map, oneOf, andThen

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


{-| Simple type alias for functions that parse a JsonValue into a value of
some arbitrary type t. Since the parse may fail, the function returns a
Result that could indicate a parse error.
-}
type alias SDecoder t =
    JsonValue
    -> Result String t


{-| Decodes a JSON string to an elm string value.
-}
string : SDecoder String
string jsonValue =
    case jsonValue of
        String s ->
            Ok s

        _ ->
            Err "Expected a string"


{-| Decodes a JSON int to an elm int value.
-}
int : SDecoder Int
int jsonValue =
    case jsonValue of
        Int i ->
            Ok i

        _ ->
            Err "Expected an integer"


{-| Decodes a JSON number to an elm float value.
-}
float : SDecoder Float
float jsonValue =
    case jsonValue of
        Int i ->
            Ok (toFloat i)

        Float f ->
            Ok f

        _ ->
            Err "Expected a number"


{-| Decodes a JSON bool to an elm bool value.
-}
bool : SDecoder Bool
bool jsonValue =
    case jsonValue of
        Bool b ->
            Ok b

        _ ->
            Err "Expected a boolean"


{-| Decodes JSON null to a supplied elm value.
-}
null : t -> SDecoder t
null val jsonValue =
    case jsonValue of
        Null ->
            Ok val

        _ ->
            Err "Expected null"


{-| Returns the value at the field with the given name.
-}
field : String -> SDecoder t -> SDecoder t
field name decoder jsonValue =
    case jsonValue of
        Obj dict ->
            case Dict.get name dict of
                Just fieldValue ->
                    decoder fieldValue

                Nothing ->
                    Err ("Field name " ++ name ++ " not found")

        _ ->
            Err "Value was not an object"


{-| Decodes a nested JSON object, following the given fields.
-}
at : List String -> SDecoder t -> SDecoder t
at path decoder v =
    case path of
        [] ->
            decoder v

        first :: rest ->
            case v of
                Obj d ->
                    case Dict.get first d of
                        Just subvalue ->
                            at rest decoder subvalue

                        Nothing ->
                            Err ("Subpath " ++ first ++ " not found")

                _ ->
                    Err "Encountered a non-object"


orElse : (() -> Result x y) -> Result x y -> Result x y
orElse thunk result =
    case result of
        Ok _ ->
            result

        Err _ ->
            thunk ()


{-| Returns a decoder that returns the result of applying the given
function to the successful result of decoding using the given decoder.
-}
map : (s -> t) -> SDecoder s -> SDecoder t
map f decoder jsonValue =
    decoder jsonValue
        |> Result.map f


{-| Returns a decoder that tries each of the given decoders in turn when
parsing its input, and that fails only if every decoder fails.
-}
oneOf : List (SDecoder t) -> SDecoder t
oneOf decoders jsonValue =
    case decoders of
        [] ->
            Err "No decoder matched value"

        decoder :: rest ->
            decoder jsonValue |> orElse (\_ -> oneOf rest jsonValue)


{-| Returns a decoder that runs the given decoder against its input. Then,
if the decode is successful, it applies the given function and re-parses
the input JSON against the resulting second decoder. This allows a decoder
that reads part of the JSON object before deciding how to parse the rest
of the object.
-}
andThen : (a -> SDecoder b) -> SDecoder a -> SDecoder b
andThen toB aDecoder jsonValue =
    case aDecoder jsonValue of
        Ok a ->
            (toB a) jsonValue

        Err s ->
            Err s
