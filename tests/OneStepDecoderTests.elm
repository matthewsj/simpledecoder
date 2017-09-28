module OneStepDecoderTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float, int, list, string)
import Test exposing (..)
import Json.Encode as Encode
import JsonUtil exposing (jsonFromString)
import OneStepDecoder exposing (..)


suite : Test
suite =
    describe "OneStepDecoder"
        [ describe "readJsonValue"
            [ fuzz string "Decodes string" <|
                \str ->
                    Expect.equal (String str) (readJsonValue (Encode.string str))
            , fuzz int "Decodes int" <|
                \i ->
                    Expect.equal (Int i) (readJsonValue (Encode.int i))
            , fuzz float "Decodes float" <|
                \f ->
                    -- JSON is lossy. 0.0 equals 0; type information is not preserved.
                    if toFloat (truncate f) == f then
                        Expect.equal (Int (truncate f)) (readJsonValue (Encode.float f))
                    else
                        Expect.equal (Float f) (readJsonValue (Encode.float f))
            , test "Decodes true" <|
                \_ ->
                    Expect.equal (Bool True) (readJsonValue (Encode.bool True))
            , test "Decodes false" <|
                \_ ->
                    Expect.equal (Bool False) (readJsonValue (Encode.bool False))
            , test "Decodes null" <|
                \_ ->
                    Expect.equal Null (readJsonValue Encode.null)
            , test "Decodes objects" <|
                \_ ->
                    case readJsonValue (jsonFromString "{\"x\": 17}") of
                        Obj d ->
                            Expect.true "Expects to have member named 'x'" (Dict.member "x" d)

                        _ ->
                            Expect.fail "Expected an object"
            , test "Decodes arrays" <|
                \_ ->
                    case readJsonValue (jsonFromString "[1,2,3,4]") of
                        Arr lst ->
                            List.map readJsonValue lst
                                |> Expect.equal [ Int 1, Int 2, Int 3, Int 4 ]

                        _ ->
                            Expect.fail "Expected an array"
            ]
        ]
