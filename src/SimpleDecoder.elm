module SimpleDecoder
    exposing
        ( JsonValue(..)
        , readJsonValue
        , toJsonValue
        , wrapPort
        , SimpleDecoder
        , string
        , int
        , float
        , bool
        , null
        , field
        , at
        , list
        , map
        , map2
        , oneOf
        , andThen
        , succeed
        , fail
        )

{-| Library for writing simple JSON decoders.

@docs JsonValue, readJsonValue, toJsonValue, wrapPort

Functions and types for manipulating JsonValue values:
@docs SimpleDecoder, string, int, float, bool, null, field, at, list, map, map2, oneOf, andThen, succeed, fail

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


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
    Decode.lazy
        (\() ->
            Decode.oneOf
                [ toJsonString
                , toJsonNumber
                , toJsonObject
                , toJsonArray
                , toJsonBool
                , toJsonNull
                ]
        )


toJsonString : Decoder JsonValue
toJsonString =
    Decode.map String Decode.string


toJsonNumber : Decoder JsonValue
toJsonNumber =
    Decode.oneOf
        [ Decode.map Int Decode.int
        , Decode.map Float Decode.float
        ]


toJsonObject : Decoder JsonValue
toJsonObject =
    Decode.map Obj (Decode.dict toJsonValue)


toJsonArray : Decoder JsonValue
toJsonArray =
    Decode.map Arr (Decode.list toJsonValue)


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


{-| Reads a JSON boolean to a Bool. Returns an error if the input isn't a boolean.
-}
bool : JsonValue -> Result String Bool
bool jsonValue =
    case jsonValue of
        Bool b ->
            Ok b

        _ ->
            Err ("Expected a boolean, got " ++ toString jsonValue)


{-| Reads a JSON string to a String. Returns an error if the input isn't a string.
-}
string : JsonValue -> Result String String
string jsonValue =
    case jsonValue of
        String s ->
            Ok s

        _ ->
            Err ("Expected a string, got " ++ toString jsonValue)


{-| Reads a JSON int to an Int. Returns an error if the input isn't an integer.
-}
int : JsonValue -> Result String Int
int jsonValue =
    case jsonValue of
        Int i ->
            Ok i

        _ ->
            Err ("Expected an integer, got " ++ toString jsonValue)


{-| Reads a JSON number to a Float. Returns an error if the input isn't a number.
-}
float : JsonValue -> Result String Float
float jsonValue =
    case jsonValue of
        Int i ->
            Ok (toFloat i)

        Float f ->
            Ok f

        _ ->
            Err ("Expected a number, got " ++ toString jsonValue)


{-| Decodes JSON null to a given value. Returns an error if the input isn't null.
-}
null : t -> JsonValue -> Result String t
null val jsonValue =
    case jsonValue of
        Null ->
            Ok val

        _ ->
            Err ("Expected null, got " ++ toString jsonValue)



-- These all have the same type! Let's abstract that out.


{-| Simple type alias for functions that parse a JsonValue into a value of
some arbitrary type t. Since the parse may fail, the function returns a
Result that could indicate a parse error.
-}
type alias SimpleDecoder t =
    JsonValue -> Result String t



-- Now let's write some higher-order parsers that help us work with non-primitive values.


{-| Decodes the named field of the given JSON object using the given decoder.
Returns an error if the given value isn't a JSON object.
-}
field : String -> SimpleDecoder t -> SimpleDecoder t
field name decoder jsonValue =
    case jsonValue of
        Obj dict ->
            case Dict.get name dict of
                Just fieldValue ->
                    decoder fieldValue

                Nothing ->
                    Err ("Expected an object with field \"" ++ name ++ "\", got: " ++ toString jsonValue)

        _ ->
            Err ("Expected an object, got: " ++ toString jsonValue)


{-| Decodes a nested JSON object, following the given fields.
-}
at : List String -> SimpleDecoder t -> SimpleDecoder t
at path decoder jsonValue =
    case path of
        [] ->
            decoder jsonValue

        first :: rest ->
            case jsonValue of
                Obj d ->
                    case Dict.get first d of
                        Just subvalue ->
                            at rest decoder subvalue

                        Nothing ->
                            Err ("Subpath " ++ first ++ " not found")

                _ ->
                    Err "Encountered a non-object"


collapseResults : List (Result String a) -> Result String (List a)
collapseResults results =
    let
        collapseResultsAcc accumulatedList results =
            case results of
                [] ->
                    Ok (List.reverse accumulatedList)

                first :: rest ->
                    case first of
                        Ok value ->
                            collapseResultsAcc (value :: accumulatedList) rest

                        Err str ->
                            Err str
    in
        collapseResultsAcc [] results


{-| Decodes a JSON array, with each element decoded by the given decoder.
-}
list : SimpleDecoder a -> SimpleDecoder (List a)
list elementDecoder jsonValue =
    case jsonValue of
        Arr jsonValues ->
            collapseResults (List.map elementDecoder jsonValues)

        _ ->
            Err ("Expected an array, got " ++ toString jsonValue)


{-| Returns a decoder that returns the result of applying the given
function to the successful result of decoding using the given decoder.
-}
map : (a -> t) -> SimpleDecoder a -> SimpleDecoder t
map f decoder jsonValue =
    decoder jsonValue
        |> Result.map f


{-| Returns a decoder that returns the result of applying the given
function to the successful result of decoding using both of the given
decoders.
-}
map2 : (a -> b -> t) -> SimpleDecoder a -> SimpleDecoder b -> SimpleDecoder t
map2 f aDecoder bDecoder jsonValue =
    case ( aDecoder jsonValue, bDecoder jsonValue ) of
        ( Ok a, Ok b ) ->
            Ok (f a b)

        ( Err s, _ ) ->
            Err s

        ( _, Err s ) ->
            Err s


{-| Returns a decoder that tries each of the given decoders in turn when
parsing its input, and that fails only if every decoder fails.
-}
oneOf : List (SimpleDecoder t) -> SimpleDecoder t
oneOf decoders jsonValue =
    let
        orElse : (() -> Result x y) -> Result x y -> Result x y
        orElse thunk result =
            case result of
                Ok _ ->
                    result

                Err _ ->
                    thunk ()
    in
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
andThen : (a -> SimpleDecoder b) -> SimpleDecoder a -> SimpleDecoder b
andThen toB aDecoder jsonValue =
    case aDecoder jsonValue of
        Ok a ->
            (toB a) jsonValue

        Err s ->
            Err s


andThen2 : (a -> SimpleDecoder b) -> SimpleDecoder a -> SimpleDecoder b
andThen2 toB aDecoder jsonValue =
    aDecoder jsonValue
        |> Result.map toB
        |> Result.andThen (\f -> f jsonValue)

{-| Returns a decoder that always succeeds with the given value.
-}
succeed : a -> SimpleDecoder a
succeed a jsonValue =
    Ok a


{-| Returns a decoder that always fails with the given error message.
-}
fail : String -> SimpleDecoder a
fail str jsonValue =
    Err str


-- Interoperability with "real" decoders


{-| Converts a simple decoder to a "real" decoder
-}
convertToDecoder : SimpleDecoder a -> Decoder a
convertToDecoder sdecoder =
    toJsonValue
        |> Decode.andThen
            (\jsonValue ->
                case sdecoder jsonValue of
                    Ok result ->
                        Decode.succeed result

                    Err msg ->
                        Decode.fail msg
            )
