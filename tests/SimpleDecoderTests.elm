module SimpleDecoderTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float, int, list, string)
import Test exposing (..)
import Json.Encode as Encode
import JsonUtil exposing (jsonFromString)
import SimpleDecoder exposing (..)


suite : Test
suite =
    describe "SimpleDecoder"
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
                            Expect.equal [ Int 1, Int 2, Int 3, Int 4 ] lst

                        _ ->
                            Expect.fail "Expected an array"
            , test "Test a real value" <|
                \_ ->
                    let
                        giphyString =
                            "{\"data\":{\"type\":\"gif\",\"id\":\"3oEjHSaIF3Oo9LjjXO\",\"url\":\"http://giphy.com/gifs/tacocatband-dana-hardly-art-tacocat-3oEjHSaIF3Oo9LjjXO\",\"image_original_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/giphy.gif\",\"image_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/giphy.gif\",\"image_mp4_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/giphy.mp4\",\"image_frames\":\"14\",\"image_width\":\"450\",\"image_height\":\"255\",\"fixed_height_downsampled_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/200_d.gif\",\"fixed_height_downsampled_width\":\"353\",\"fixed_height_downsampled_height\":\"200\",\"fixed_width_downsampled_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/200w_d.gif\",\"fixed_width_downsampled_width\":\"200\",\"fixed_width_downsampled_height\":\"113\",\"fixed_height_small_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/100.gif\",\"fixed_height_small_still_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/100_s.gif\",\"fixed_height_small_width\":\"176\",\"fixed_height_small_height\":\"100\",\"fixed_width_small_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/100w.gif\",\"fixed_width_small_still_url\":\"https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/100w_s.gif\",\"fixed_width_small_width\":\"100\",\"fixed_width_small_height\":\"57\",\"username\":\"tacocatband\",\"caption\":\"\"},\"meta\":{\"status\":200,\"msg\":\"OK\",\"response_id\":\"59cc4fae446c536c7742d0b2\"}}"

                        parsed =
                            readJsonValue (jsonFromString giphyString)
                    in
                        Expect.equal (at [ "data", "image_url" ] parsed)
                            (Ok (String "https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/giphy.gif"))
            ]
        ]
