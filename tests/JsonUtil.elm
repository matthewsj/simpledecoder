module JsonUtil exposing (..)

import Json.Decode as Decode

{-| Returns the JSON value corresponding to the given JSON string.

Crashes if the string is not a valid JSON string. Only use this function for testing.
-}
jsonFromString : String -> Decode.Value
jsonFromString s =
    case Decode.decodeString Decode.value s of
        Ok v ->
            v

        Err err ->
            Debug.crash ("Couldn't read string: " ++ err)

