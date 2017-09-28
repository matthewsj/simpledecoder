module OneStepDecoder exposing (OneStepValue(..), readJsonValue, toOneStepValue)

{-| A simple alternative decoder for JSON values that plays nicely with decoders from
other libraries. To use, call readJsonValue and process the result as you would any other
elm value, recursively processing JSON values created with readJsonValue. To incorporate
existing JSON decoders, invoke them using Json.Decode.decodeValue at appropriate points
in your function.

@docs OneStepValue, readJsonValue, toOneStepValue

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, value)


{-| Json type that represents decoding a single layer.
-}
type OneStepValue
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Null
    | Obj (Dict String Decode.Value)
    | Arr (List Decode.Value)


{-| Function that converts a JSON value to a OneStepValue.
-}
readJsonValue : Decode.Value -> OneStepValue
readJsonValue v =
    case Decode.decodeValue toOneStepValue v of
        Ok oneStepValue ->
            oneStepValue

        Err s ->
            Debug.crash ("Error reading JSON value generically: " ++ s)


{-| Decoder that turns JSON into OneStepValue objects that can be further parsed by client code.
-}
toOneStepValue : Decoder OneStepValue
toOneStepValue =
    Decode.oneOf
        [ toOneStepString
        , toOneStepNumber
        , toOneStepObject
        , toOneStepArray
        , toOneStepBool
        , toOneStepNull
        ]


toOneStepString : Decoder OneStepValue
toOneStepString =
    Decode.map String Decode.string


toOneStepNumber : Decoder OneStepValue
toOneStepNumber =
    Decode.oneOf
        [ Decode.map Int Decode.int
        , Decode.map Float Decode.float
        ]


toOneStepObject : Decoder OneStepValue
toOneStepObject =
    Decode.map Obj (Decode.lazy (\_ -> Decode.dict value))


toOneStepArray : Decoder OneStepValue
toOneStepArray =
    Decode.map Arr (Decode.lazy (\_ -> Decode.list value))


toOneStepBool : Decoder OneStepValue
toOneStepBool =
    Decode.map Bool Decode.bool


toOneStepNull : Decoder OneStepValue
toOneStepNull =
    Decode.null Null


{-| Converts a Json.Decode.Value port into a OneStepValue port.
-}
wrapPort : ((Decode.Value -> msg) -> Sub msg) -> (OneStepValue -> msg) -> Sub msg
wrapPort p oneStepDecoder =
    p (readJsonValue >> oneStepDecoder)
