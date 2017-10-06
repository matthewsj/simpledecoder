module SimpleDecoderTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float, int, list, string)
import Test exposing (..)
import Json.Encode as Encode
import JsonUtil exposing (jsonFromString)
import SimpleDecoder exposing (JsonValue(..), SimpleDecoder, readJsonValue)


constructValueAtPath : JsonValue -> List String -> JsonValue
constructValueAtPath value path =
    case path of
        [] ->
            value

        first :: rest ->
            Obj (Dict.fromList [ ( first, constructValueAtPath value rest ) ])


readJsonFromString : String -> JsonValue
readJsonFromString =
    jsonFromString >> readJsonValue



{-

      {
        "operations": [
          {
            "action": "createEnemy",
            "name": "Zombie",
            "hitPoints": 4
          }, {
            "action": "movePlayer",
            "location": "forest"
          }
        ]
      }


   type Operation
       = CreateEnemy { id : Int, hitPoints : Int }
       | MovePlayer { location : String }


         We'd like a function

            JsonValue -> List Operation

         Oh, but of course we might fail to parse, so we'd better make it

            JsonValue -> Result String (List Operation)

-}


type Operation
    = CreateEnemy { name : String, hitPoints : Int }
    | MovePlayer { location : String }


readOperations : JsonValue -> Result String (List Operation)
readOperations jsonValue =
    case jsonValue of
        Obj obj ->
            case Dict.get "operations" obj of
                Just opsList ->
                    readOperationsList opsList

                Nothing ->
                    Err ("Expected an object with field \"operations\", got: " ++ toString jsonValue)

        _ ->
            Err ("Expected an object, got " ++ toString jsonValue)


readOperationsList : JsonValue -> Result String (List Operation)
readOperationsList jsonValue =
    case jsonValue of
        Arr arr ->
            collapseResults (List.map readOperation arr)

        _ ->
            Err ("Expected an array, got " ++ toString jsonValue)


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


readOperation : JsonValue -> Result String Operation
readOperation jsonValue =
    case jsonValue of
        Obj fields ->
            case Dict.get "action" fields of
                Just (String "createEnemy") ->
                    readCreateEnemy fields

                Just (String "movePlayer") ->
                    readMovePlayer fields

                Just (String s) ->
                    Err ("Got invalid action: " ++ s)

                Just v ->
                    Err ("Expected a string, got: " ++ toString v)

                Nothing ->
                    Err ("Expected an object with field \"action\", got: " ++ toString jsonValue)

        _ ->
            Err ("Expected an object, got: " ++ toString jsonValue)


readCreateEnemy : Dict String JsonValue -> Result String Operation
readCreateEnemy fields =
    case Dict.get "name" fields of
        Just (String name) ->
            case Dict.get "hitPoints" fields of
                Just (Int hitPoints) ->
                    Ok (CreateEnemy { name = name, hitPoints = hitPoints })

                Just v ->
                    Err ("Expected an integer, got: " ++ toString v)

                Nothing ->
                    Err "Expected an object with field \"hitPoints\""

        Just v ->
            Err ("Expected a string, got: " ++ toString v)

        Nothing ->
            Err "Expected an object with field \"name\""


readMovePlayer : Dict String JsonValue -> Result String Operation
readMovePlayer fields =
    case Dict.get "location" fields of
        Just (String location) ->
            Ok (MovePlayer { location = location })

        Just v ->
            Err ("Expected a string, got: " ++ toString v)

        Nothing ->
            Err "Expected an object with field \"location\""


decodeOperations : SimpleDecoder (List Operation)
decodeOperations =
    SimpleDecoder.list <|
        SimpleDecoder.field "action" SimpleDecoder.string
            |> SimpleDecoder.andThen
                (\action ->
                    case action of
                        "createEnemy" ->
                            SimpleDecoder.map2
                                (\name hitPoints ->
                                    CreateEnemy
                                        { name = name
                                        , hitPoints = hitPoints
                                        }
                                )
                                (SimpleDecoder.field "name" SimpleDecoder.string)
                                (SimpleDecoder.field "hitPoints" SimpleDecoder.int)

                        "movePlayer" ->
                            SimpleDecoder.map
                                (\location -> MovePlayer { location = location })
                                (SimpleDecoder.field "location" SimpleDecoder.string)

                        _ ->
                            SimpleDecoder.fail ("Got invalid action: " ++ action)
                )


type Pet
    = Dog { name : String }
    | Cat { name : String, lives : Int }


decodePetsAttempt1 : JsonValue -> Result String (List Pet)
decodePetsAttempt1 jsonValue =
    case jsonValue of
        Arr values ->
            List.map decodePetAttempt1 values |> collapseResults

        _ ->
            Err "Expected an array, got a non-array"


decodePetAttempt1 : JsonValue -> Result String Pet
decodePetAttempt1 jsonValue =
    case jsonValue of
        Obj fields ->
            case Dict.get "dog" fields of
                Just dogValue ->
                    case dogValue of
                        String s ->
                            Ok (Dog { name = s })

                        _ ->
                            Err "dog didn't have a name!"

                Nothing ->
                    case Dict.get "cat" fields of
                        Just catValue ->
                            case ( catValue, Dict.get "name" fields ) of
                                ( Obj catFields, Just (String name) ) ->
                                    case Dict.get "lives" catFields of
                                        Just (Int lives) ->
                                            Ok (Cat { name = name, lives = lives })

                                        _ ->
                                            Err "Bad case"

                                _ ->
                                    Err "Also bad"

                        _ ->
                            Err "I give up"

        _ ->
            Err "Please stop"


decodePets : SimpleDecoder (List Pet)
decodePets =
    SimpleDecoder.list decodePet


decodePet : SimpleDecoder Pet
decodePet =
    SimpleDecoder.oneOf
        [ SimpleDecoder.map
            (\name -> Dog { name = name })
            (SimpleDecoder.field "dog" SimpleDecoder.string)
        , SimpleDecoder.map2
            (\name lives -> Cat { name = name, lives = lives })
            (SimpleDecoder.field "name" SimpleDecoder.string)
            (SimpleDecoder.at [ "cat", "lives" ] SimpleDecoder.int)
        ]


expectParses : List (SimpleDecoder a) -> String -> a -> Expectation
expectParses decoders jsonString expected =
    readJsonFromString jsonString
        |> Expect.all
            (List.map
                (\decoder val -> Expect.equal (Ok expected) (decoder val))
                decoders
            )


expectPetParse : String -> Pet -> Expectation
expectPetParse =
    expectParses [ decodePetAttempt1, decodePet ]


expectOperationsParse : String -> List Operation -> Expectation
expectOperationsParse =
    expectParses [ readOperations, decodeOperations ]


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
                    case readJsonFromString "{\"x\": 17}" of
                        Obj d ->
                            Expect.true "Expects to have member named 'x'" (Dict.member "x" d)

                        _ ->
                            Expect.fail "Expected an object"
            , test "Decodes arrays" <|
                \_ ->
                    case readJsonFromString "[1,2,3,4]" of
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
                            readJsonFromString giphyString
                    in
                        Expect.equal (SimpleDecoder.at [ "data", "image_url" ] Ok parsed)
                            (Ok (String "https://media2.giphy.com/media/3oEjHSaIF3Oo9LjjXO/giphy.gif"))
            , fuzz (list string) "Test at on arbitrary paths" <|
                \path ->
                    let
                        nestedValue =
                            String "testValue"

                        jsonValue =
                            constructValueAtPath nestedValue path
                    in
                        Expect.equal (Ok nestedValue) (SimpleDecoder.at path Ok jsonValue)
            , fuzz string "string decoder" <|
                \s ->
                    Expect.equal (Ok s) (SimpleDecoder.string (String s))
            , fuzz int "int decoder" <|
                \i ->
                    Expect.equal (Ok i) (SimpleDecoder.int (Int i))
            , fuzz float "float decoder on float input" <|
                \f ->
                    Expect.equal (Ok f) (SimpleDecoder.float (Float f))
            , fuzz int "float decoder on int input" <|
                \i ->
                    Expect.equal (Ok (toFloat i)) (SimpleDecoder.float (Int i))
            , fuzz (list int) "decodes lists of integers" <|
                \ints ->
                    SimpleDecoder.list SimpleDecoder.int (Arr (List.map Int ints))
                        |> Expect.equal (Ok ints)
            , test "oneOf" <|
                \_ ->
                    SimpleDecoder.oneOf
                        [ SimpleDecoder.int
                        , SimpleDecoder.at [ "a", "b" ] SimpleDecoder.int
                        ]
                        (readJsonFromString "{\"a\": {\"b\": 12}}")
                        |> Expect.equal (Ok 12)
            , let
                decoder =
                    SimpleDecoder.field "interestingVar" SimpleDecoder.string
                        |> SimpleDecoder.andThen
                            (\interestingVar ->
                                SimpleDecoder.field interestingVar SimpleDecoder.int
                            )
              in
                test "andThen" <|
                    \_ ->
                        decoder (readJsonFromString "{\"interestingVar\":\"b\",\"a\":10,\"b\":12}")
                            |> Expect.equal (Ok 12)
            ]
        , test "Dog parsing" <|
            \_ -> expectPetParse "{\"dog\":\"Fido\"}" (Dog { name = "Fido" })
        , test "Cat parsing" <|
            \_ -> expectPetParse "{\"cat\":{\"lives\":3},\"name\":\"Sparky\"}" (Cat { name = "Sparky", lives = 3 })
        , test "Operations parsing" <|
            let
                json =
                    """
                        {
                          "operations": [
                            {
                              "action": "createEnemy",
                              "name": "Zombie",
                              "hitPoints": 4
                            }, {
                              "action": "movePlayer",
                              "location": "forest"
                            }
                          ]
                        }
                        """

                expected =
                    [ CreateEnemy { name = "Zombie", hitPoints = 4 }
                    , MovePlayer { location = "forest" }
                    ]
            in
                \_ ->
                    expectOperationsParse json expected
        , test "Operations failures" <|
            let
                json =
                    """
                        {
                          "operations": [
                            {
                              "action": "createEnemy",
                              "name": "Zombie",
                              "hitPoints": "not a number"
                            }, {
                              "action": "movePlayer",
                              "location": "forest"
                            }
                          ]
                        }
                        """

                expected =
                    [ CreateEnemy { name = "Zombie", hitPoints = 4 }
                    , MovePlayer { location = "forest" }
                    ]
            in
                \_ ->
                    readOperations (readJsonFromString json)
                        |> Expect.equal (Err "Expected an integer, got: String \"not a number\"")
        ]
