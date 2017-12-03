# You could have designed the `Json.Decode` library!

A lot of people who are learning elm for the first time find the elm's
`Json.Decode` library to be a big stumbling block. It's the only way to write
JSON values from elm, so you're probably going to have to use it if you want
your elm program to talk to the outside world.

But why does it have to be so, well, _weird_?

You've got the hang of how to define records and tagged unions already, and you
had to learn how to use `case` and pattern matching to read complicated data
structures already -- what's up with `Json.Decode` making you build up decoders
using a special set of functions like `field` and `oneOf`? Why can't we just
have a normal elm datatype that represents a JSON value and write normal elm
functions to process it?

Let's find out.

In this article, I'm going to try to lead you to designing elm's `Json.Decode`
library. I'll assume you're comfortable writing elm and you know what JSON is in
general, but that you don't know anything about how decoding works. By the end,
I hope you'll understand how `Json.Decode` works and have an intuition for why
it's designed the way it is.

## JSON the normal way

Let's pretend that instead of `Json.Decode.Value`, we had a completely normal type defined as:
```elm
type JsonValue
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Null
    | Obj (Dict String JsonValue)
    | Arr (List JsonValue)
```

A JSON value is a string, a number (`Int` or `Float`), a boolean, the value
`null`, an object consisting of strings associated with other JSON values, or an
array of JSON values. This is how you might expect JSON to be exposed in elm,
and in any event it's totally bog-standard elm code with nothing fancy going on.
Easy peasy.

Let's say we're writing a game client, and the server sends us groups of updates
periodically in the form of a JSON message that looks like this:

```json
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
```
We need to use these in elm somehow. We could use the `JsonValue` representation
directly, but that seems like a bad idea since it'd mean we'd have to deal with
the details of the JSON format all through our program. Instead we should define
a more reasonable elm type like this:
```elm
type Operation
    = CreateEnemy { name : String, hitPoints : Int }
    | MovePlayer { location : String }
```
and write a function that translates a `JsonValue` and produces a list of into a
list of `Operation`s:
```elm
readOperations : JsonValue -> List Operation
```

Oh, and we also need to account for what happens if we get a JSON value that
isn't a legal operation. We could return a `Maybe (List Operation)`, but for
debugging we're probably going to want to know why the JSON we got wasn't an
operation, so let's make it a `Result String (List Operation)` instead. So we
want to write:

```elm
readOperations : JsonValue -> Result String (List Operation)
```

Since we're talking about `JsonValue`s, this is just a normal elm function, we
can write it out using just `case`s and normal elm things. Below is a first
stab. (I'm going to show the whole program, but it's big and I'll talk about the
important parts directly, so don't feel like you need to study it.)
```elm
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
```

Holy cow, this function is _huge!_ Somehow I needed nearly 100 lines of code
just to read a pretty simple bit of JSON. And if the format were more
complicated, this code would keep getting bigger.

## What's going on here?

If you look the program over, you can see why. When we read from JSON, every step of the way there are things that could go wrong, and elm's type system (rightly) forces us to handle every possible error. That leads to a lot of boilerplate.

For instance, in `readCreateEnemy`, the first case of the first case handles the good path, and the entire rest of the function -- four more cases -- mechanically address all the ways reading could go wrong. This not only makes the code tedious to write, it also makes it tedious to _read_.

The problem is that the straightforward program we've written combines the
details of two different concerns:
* What we should do in the "good case" to turn our `JsonValue` into an
`Operation` assuming the JSON has the shape we expect. This concern is specific
to our particular program and the way we want to translate our expected JSON
format to our program-specific `Operation` type.
* How to detect if the input JSON doesn't have the expected shape and if so how
to construct an appropriate error. This concern is _not_ specific to anything in
our program. It seems likely that almost any program we write that involves
parsing JSON is going to have some kind of expected shape and will want to
signal an error if the input doesn't conform to it.

The second concern is legitimate, of course -- we need to deal
with the fact that our input JSON might not have the shape we expect. But that
concern dominates so much of the code, and is so generic and boilerplate-ey,
that it makes it hard to understand what our decoder does _other_ than find
format errors. So let's see if we can find a way to separate out the code for
our two concerns so that we can put the error handling code into its own
library.

## Pulling out all that error handling code

Notice how every function starts with a `case` statement that checks that the
JSON object is the appropriate tag, and returns an error message otherwise?
Let's write some helper functions that just do that. Let's start with the
primitive values that have straightforward elm equivalents:
```elm
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

-- ... and similar for float and bool

```

Before we move on, one observation: We're writing the type `JsonValue -> Result
String [something]` a lot to represent things that read a JSON value to some elm
type. When I see a common pattern in types like this, I like to give it a name
to help me think about it:

```elm
{-| Simple type alias for functions that parse a JsonValue into a value of
some arbitrary type t. Since the parse may fail, the function returns a
Result that could indicate a parse error.
-}
type alias JsonDecoder t = JsonValue -> Result String t
```
From now on I'll use this type. But when you see it, remember that it's just a
function that reads a `JsonValue` and produces a result. We're still in the
world of plain old elm functions and data structures.

## Arrays

Now we've gotten rid of all that error handling boilerplate for all the flat
JSON values, but we couldn't handle many interesting JSON values without also
dealing with arrays and objects. Up to this point, the functions we've been
writing only needed to do one thing: look at one level of JSON structure and
either return the equivalent elm value or an error. But arrays and objects are
containers for more JSON values that the caller probably has an opinion about:
we don't just want a list, we want a list of strings in one place and maybe a
list of integers in another.

Fortunately, elm makes it easy to snap functions together to build bigger
functions, so let's make our array decoder take another decoder that it will use
to decode each element. We can just use the implementation of
`readOperationsList` we wrote earlier and modify it to take a decoder argument:
```elm
{-| Decodes a JSON array, with each element decoded by the given decoder.
-}
list : JsonDecoder a -> JsonDecoder (List a)
list elementDecoder jsonValue =
    case jsonValue of
        Arr jsonValues ->
            collapseResults (List.map elementDecoder jsonValues)
        _ ->
            Err ("Expected an array, got " ++ toString jsonValue)


{-| Groups a list of Results together into a result that either produces all the
successes in a list if all are Ok, or the first error if there are any.
-}
collapseResults : List (Result String t) -> Result String (List t)
collapseResults = ... -- see above for implementation
```
That's it!

## Objects

Objects pose some problems of their own.

* For one thing, JSON objects have fields, and when we process an object we
  almost always want to get a specific named field and process that.
* For another, when we read an object like
 ```json
{
  "action": "createEnemy",
  "name": "Zombie",
  "hitPoints": 4
}
 ```
 we need to read multiple fields off of it and provide them to some other
function to get a result -- in this case we need to read the `name` and
`hitPoints` fields and provide them to `CreateEnemy`.
* Finally, we may need to figure out what to expect from an object by reading
  other pieces of it. For our operations, we need to read the `"action"` field
  to know whether we're making a `CreateEnemy` or a `MovePlayer` action and what
  other fields to expect to see on the object.

Let's tackle these one at a time.

### Reading fields

Now that we've dealt with arrays, this seems pretty straightforward. We can do
what we did there: write a function that takes a `JsonDecoder` and a field
name as arguments, and returns a new `JsonDecoder` that expects to see an
object that contains the given field and reads it with the given decoder. Here's
how that looks:

```elm
{-| Decodes the named field of the given JSON object using the given decoder.
Returns an error if the given value isn't a JSON object.
-}
field : String -> JsonDecoder t -> JsonDecoder t
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
```

### Combining multiple fields

In our first version of `decodeOperations`, the function `readCreateEnemy` is
responsible for reading the `name` and `hitPoints` fields of a JSON object and
using the contents to build a `CreateEnemy` operation. It is easily the nastiest
function in that implementation due to how much error handling we need to do, so
if we're trying to clean up the error-handling code we're going to have to do
something here. But it's not immediately apparent _what_.

Let's think about what we're going to need to do. First of all,
`readCreateEnemy`'s job is to build a `CreateEnemy`, a value that has nothing to
do with JSON; it seems like a good idea to let the caller provide a function
that does the combining while the boilerplate we're writing now handles calling
it under the right circumstances. Since our job is to handle the errors, that
function should just take the successful results of decoding the subfields, so
we're also going to need the caller to tell us what subfields they want and how
to decode their contents.

Let's write that out (pretending for the sake of argument we always want to read
and combine exactly two fields):
```elm
potentialMultifieldDecoder :
    (a -> b -> t)
    -> String
    -> JsonDecoder a
    -> String
    -> JsonDecoder b
    -> JsonDecoder t
```
This seems like a function we could implement, but it's frustrating that we
_just_ wrote a function that decodes a field, and we're going to have to write
it again in the body of this new function. This is one of these nice situations
where we can do less work and make our library more powerful at the same time --
instead of forcing the caller to read fields from an object, let's allow them to
decode anything they want! They can easily decode multiple fields from an object
using the `field` function we just wrote, or they can decode and combine
anything else they want.

That function becomes:
```elm
{-| Returns a decoder that returns the result of applying the given
function to the successful result of decoding using both of the given
decoders.
-}
object2 : (a -> b -> t) -> JsonDecoder a -> JsonDecoder b -> JsonDecoder t
object2 f aDecoder bDecoder jsonValue =
    case ( aDecoder jsonValue, bDecoder jsonValue ) of
        ( Ok a, Ok b ) ->
            Ok (f a b)

        ( Err s, _ ) ->
            Err s

        ( _, Err s ) ->
            Err s

```
We can also define `object3`, `object4`, and so on.

Looking at what we've written, does the type look familiar to you? If you read a
lot of elm library code, you might have noticed that it fits the `map` pattern
that shows up in a lot of libraries: for instance,
```elm
Maybe.map2 : (a -> b -> t) -> Maybe a -> Maybe b -> Maybe t
Result.map2 : (a -> b -> t) -> Result x a -> Result x b -> Result x t
Task.map2 : (a -> b -> t) -> Task x a -> Task x b -> Task x t
```

In all those libraries, a "map" function does the same kind of thing. We have
some type like `Maybe` that we can think of as a "sort-of `a`" type -- `Maybe a`
is an `a` that might not be there, `Result x a` is an `a` that might be an error
value of type `x` instead, and `Task x a` is a recipe for an asynchronous task
that might produce an `a` if we execute it. That makes `map2` a "sort-of
function application" given that we have a sort-of `a` and a sort-of `b`
already, apply the function to the underlying `a` and `b` values if and once
they exist. Since they're only sort of values, the result is also only sort of a
value.

In our case, `JsonDecoder a` represents a "sort-of `a`" -- an `a` that we
might get by reading it out of a JSON value. It fits the pattern nicely! In that
context, it's clear that `object2` is really just `map2`, and we should call it
that for consistency with other elm libraries. Also, it makes it obvious that
there's a good reason for us to add a `map` (i.e., `map1`) that transforms just
a single argument. We can use this for more than just reading fields off of an
object!

```elm
map2 : (a -> b -> t) -> JsonDecoder a -> JsonDecoder b -> JsonDecoder t
map2 = object2


{-| Returns a decoder that returns the result of applying the given
function to the successful result of decoding using the given decoder.
-}
map : (a -> t) -> JsonDecoder a -> JsonDecoder t
map f decoder jsonValue =
    decoder jsonValue
        |> Result.map f
```

### Making decisions while decoding

At this point we've built up a library that factors out error handling nicely
for _almost_ everything we did in our original program. The one problem we have
left is our original `readOperation` function, which looks at the `action` field
and decides whether to call `readCreateEnemy` or `readMovePlayer`. If we think
of `readOperation` as being composed of the "real function" and the "error
boilerplate," then the real function is the part that reads the `action` field
as a string and then performs case dispatch on it to call either
`readCreateEnemy` or `readMovePlayer`, and the error boilerplate is the series
of four cases at the end of the function that handle the scenarios:

1. When the JSON object we're reading specifies an `action` field that isn't `"createEnemy"` or `"movePlayer"`,
1. When it specifies an `action` field that isn't a string,
1. When it doesn't have an `action` field, and
1. When it isn't an object at all.

The first one is pretty specific to reading `Operation`s, but the others are
completely generic: in fact, `field "action" string` already handles all three.
So let's figure out a way to reuse that. We need the user to specify the "real
function" from above, so let's just take it directly as a (real) function. This
function should take a successfully-decoded string, but what should it return?
In the code we're trying to remove boilerplate from it returned
`readCreateEnemy` and `readMovePlayer`, both of which can be thought of as
`JsonDecoder`s. So our boilerplate could take a `JsonDecoder a` and a
function `a -> JsonDecoder b` and should return a `JsonDecoder b`. Here's
that type all at once:

```elm
(a -> JsonDecoder b) -> JsonDecoder a -> JsonDecoder b
```

That type looks familiar too! It shows up in lots of elm packages under the name
`andThen`: for instance, just in `core`, we've got

```elm
Maybe.andThen : (a -> Maybe b) -> Maybe a -> Maybe b
Result.andThen : (a -> Result x b) -> Result x a -> Result x b
Task.andThen : (a -> Task x b) -> Task x a -> Task x b
```
and on and on.

In all those cases, `andThen` does basically the same thing we want it to do
here. Remember `map` represents applying a function `a -> b` to a "sort-of `a`";
we might not not get our `a`, but if we do we can definitely convert it to a
`b`. `andThen` is for the situation where we have a sort-of `a`, and once we
actually get our hands on an `a` we want to use it to figure out how to make a
sort-of `b`. For instance, you can use
```elm
Maybe.map (\x -> x + 1) maybeNumber
```
to add 1 to `maybeNumber` if it exists, and
```elm
Maybe.andThen (\x -> if x >= 5 then (Just x) else Nothing) maybeNumber
```
to turn `maybeNumber` into `Nothing` if it's less than 5.

This pattern is exactly what we want to do with decoders, so let's call our
function `andThen`. It's easy enough to implement:

```elm
{-| Returns a decoder that runs the given decoder against its input. Then,
if the decode is successful, it applies the given function and re-parses
the input JSON against the resulting second decoder. This allows a decoder
that reads part of the JSON object before deciding how to parse the rest
of the object.
-}
andThen : (a -> JsonDecoder b) -> JsonDecoder a -> JsonDecoder b
andThen toB aDecoder jsonValue =
    case aDecoder jsonValue of
        Ok a ->
            (toB a) jsonValue

        Err s ->
            Err s
```

One last detail: Our boilerplate handles the generic problems, but using
`andThen` we might find ourselves writing logic that wants to directly return a
success with a particular value or a failure without running any more child
decoders. We can't just return `Ok` or `Error` values in those cases because
`andThen` wants us to return a `JsonDecoder` -- which, remember, is a function
that takes a `JsonValue` to a `Result`, not a `Result` itself. But we can do the
next best thing:

```elm
{-| Returns a decoder that always succeeds with the given value.
-}
succeed : a -> JsonDecoder a
succeed a jsonValue =
    Ok a


{-| Returns a decoder that always fails with the given error message.
-}
fail : String -> JsonDecoder a
fail str jsonValue =
    Err str
```

## Parsing operations, take two

Now we've written everything we need to pull out all the boilerplate from
`readOperation`. Let's see what it looks like when we take all that boilerplate
out:

```elm
readOperations : JsonDecoder (List Operation)
readOperations =
    JsonDecoder.list <|
        JsonDecoder.field "action" JsonDecoder.string
            |> JsonDecoder.andThen
                (\action ->
                    case action of
                        "createEnemy" ->
                            JsonDecoder.map2
                                (\name hitPoints ->
                                    CreateEnemy
                                        { name = name
                                        , hitPoints = hitPoints
                                        }
                                )
                                (JsonDecoder.field "name" JsonDecoder.string)
                                (JsonDecoder.field "hitPoints" JsonDecoder.int)

                        "movePlayer" ->
                            JsonDecoder.map
                                (\location -> MovePlayer { location = location })
                                (JsonDecoder.field "location" JsonDecoder.string)

                        _ ->
                            JsonDecoder.fail ("Got invalid action: " ++ action)
                )
```

Wow! This version is only about a quarter of the size of our first attempt, and
it's much more readable. If you squint a bit, it reads almost like a
straightforward description of the JSON format: It's a list of objects with
field called `action`. If `action` is `"createEnemy"` then it should have `name`
and `hitPoints` fields, if it's `"movePlayer"` then it should have a `location`
field, and anything else is illegal. I would much prefer to maintain this
version of the parse than what we started with.

It's worth reflecting on what we've done here. The only thing we did was try
to abstract away the boilerplate involved in handling the cases where we're
trying to decode a JSON value and discover it doesn't have the shape we
expected. There's no magic involved here, everything was just short, relatively straightforward code, and yet we were able to transform an unreadable mess into
something that basically makes sense, and in the process we got a nice library
that we could use for other projects.

## Congratulations! You designed `Json.Decode`!

The bad news is, the `Json.Decode` library doesn't have a `JsonValue` type like
the one we've been using here. It's implemented in native Javascript and doesn't
give you access to the underlying data structures it uses.

The good news is, every other function we've written here was actually taken
straight from the `Json.Decode` API: replace `JsonDecoder` with `Decoder` and
everything works just the same! And as I hope I've convinced you, even
`Json.Decode` _did_ give you a `JsonValue` equivalent, you'd want to use the API
functions anyway. Now go read the [rest of the API](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode): everything there is just an extension of the ideas we've worked out here.
I hope that now that you've figured out from first principles why the library
works the way it does, you'll find it a little less mysterious and easier to
work with.
