module Random.General exposing
    ( Config, makeConfig
    , Generator, step, bool, int, float, oneIn, sample
    , pair, list, maybe, choice, choices, frequency
    , constant, map, map2, map3, map4, map5, andMap, andThen, filter
    , minInt, maxInt
    , generator
    )

{-| A basis for random number generators.
You only need this if you want to use another internal random number generators than the default.
This could for instance be used to implement a cryptographically secure random number generator,
with the same functionality that you love from the elm `Random` library (e.g. Generators).

\*\* All functions that are not documented here are exactly like the ones described in the `Random.Pcg` module. \*\*


# Create custom RNGs

@docs Config, makeConfig


# Basic Generators

@docs Generator, step, bool, int, float, oneIn, sample


# Combining Generators

@docs pair, list, maybe, choice, choices, frequency


# Custom Generators

@docs constant, map, map2, map3, map4, map5, andMap, andThen, filter


# Constants

@docs minInt, maxInt


# Internal

@docs generator

-}

import Bitwise


{-| -}
type Generator s a
    = Generator (s -> ( a, s ))


{-| This allows you to create base generators such as the `int` or `float` generator.
Only use this if you really know what you are doing.
In almost all cases it is better to create generators that are based on some other generators
using the combinations functions provided in this library.

If you really need this, a generator is just a function of type `s -> (a, s)`.
E.g. a function that takes the current state of your seed, gets some value out of it and generates a new seed.

-}
generator : (s -> ( a, s )) -> Generator s a
generator =
    Generator


{-| A `Config` describes how the internal random number generator works.
see `makeConfig` for more.
-}
type Config s
    = Config { next : s -> s, peel : s -> Int }


{-| -}
step : Generator s a -> s -> ( a, s )
step (Generator gen) seed =
    gen seed


{-| `makeConfig` takes two arguments, a `next` function and a `peel` function.
The `next` function describes how to generate a new internal state from the current internal state.
The `peel` function describes how to map the internal state to a single integer.

As an example, let's see how to implement a terrible RNG:

    incrementRNG =
        makeConfig
            (\s ->
                -- Bitwise.shiftRightZfBy 0 is used to truncate the state to 32 bits
                s + 1 |> Bitwise.shiftRightZfBy 0
            )
            identity

-}
makeConfig : (s -> s) -> (s -> Int) -> Config s
makeConfig next peel =
    Config { next = next, peel = peel }


{-| -}
int : Config s -> Int -> Int -> Generator s Int
int (Config c) a b =
    Generator <|
        \seed0 ->
            let
                ( lo, hi ) =
                    if a < b then
                        ( a, b )

                    else
                        ( b, a )

                range =
                    hi - lo + 1
            in
            -- fast path for power of 2
            if (range |> Bitwise.and (range - 1)) == 0 then
                ( (c.peel seed0 |> Bitwise.and (range - 1) |> Bitwise.shiftRightZfBy 0) + lo, c.next seed0 )

            else
                let
                    threshhold =
                        -- essentially: period % max
                        remainderBy range (-range |> Bitwise.shiftRightZfBy 0) |> Bitwise.shiftRightZfBy 0

                    accountForBias : s -> ( Int, s )
                    accountForBias seed =
                        let
                            x =
                                c.peel seed

                            seedN =
                                c.next seed
                        in
                        if x < threshhold then
                            -- in practice this recurses almost never
                            accountForBias seedN

                        else
                            ( remainderBy range x + lo, seedN )
                in
                accountForBias seed0


{-| -}
float : Config s -> Float -> Float -> Generator s Float
float (Config c) min max =
    Generator <|
        \seed0 ->
            let
                -- Get 64 bits of randomness
                seed1 =
                    c.next seed0

                n0 =
                    c.peel seed0

                n1 =
                    c.peel seed1

                -- Get a uniformly distributed IEEE-754 double between 0.0 and 1.0
                hi =
                    toFloat (n0 |> Bitwise.and 0x03FFFFFF) * 1.0

                lo =
                    toFloat (n1 |> Bitwise.and 0x07FFFFFF) * 1.0

                val =
                    ((hi * bit27) + lo) / bit53

                -- Scale it into our range
                range =
                    abs (max - min)

                scaled =
                    val * range + min
            in
            ( scaled, c.next seed1 )


bit53 =
    9007199254740992.0


bit27 =
    134217728.0


{-| -}
bool : Config s -> Generator s Bool
bool c =
    map ((==) 1) (int c 0 1)


{-| -}
maxInt : Int
maxInt =
    2147483647


{-| -}
minInt : Int
minInt =
    -2147483648


{-| -}
pair : Generator s a -> Generator s b -> Generator s ( a, b )
pair genA genB =
    map2 Tuple.pair genA genB


{-| -}
list : Int -> Generator s a -> Generator s (List a)
list n (Generator generate) =
    Generator <|
        \seed ->
            listHelp [] n generate seed


listHelp : List a -> Int -> (s -> ( a, s )) -> s -> ( List a, s )
listHelp list_ n generate seed =
    if n < 1 then
        ( list_, seed )

    else
        let
            ( value, newSeed ) =
                generate seed
        in
        listHelp (value :: list_) (n - 1) generate newSeed


{-| -}
constant : a -> Generator s a
constant value =
    Generator (\seed -> ( value, seed ))


{-| -}
map : (a -> b) -> Generator s a -> Generator s b
map func (Generator genA) =
    Generator <|
        \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0
            in
            ( func a, seed1 )


{-| -}
map2 : (a -> b -> c) -> Generator s a -> Generator s b -> Generator s c
map2 func (Generator genA) (Generator genB) =
    Generator <|
        \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1
            in
            ( func a b, seed2 )


{-| -}
map3 : (a -> b -> c -> d) -> Generator s a -> Generator s b -> Generator s c -> Generator s d
map3 func (Generator genA) (Generator genB) (Generator genC) =
    Generator <|
        \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1

                ( c, seed3 ) =
                    genC seed2
            in
            ( func a b c, seed3 )


{-| -}
map4 : (a -> b -> c -> d -> e) -> Generator s a -> Generator s b -> Generator s c -> Generator s d -> Generator s e
map4 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) =
    Generator <|
        \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1

                ( c, seed3 ) =
                    genC seed2

                ( d, seed4 ) =
                    genD seed3
            in
            ( func a b c d, seed4 )


{-| Combine five generators.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Generator s a -> Generator s b -> Generator s c -> Generator s d -> Generator s e -> Generator s f
map5 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) (Generator genE) =
    Generator <|
        \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1

                ( c, seed3 ) =
                    genC seed2

                ( d, seed4 ) =
                    genD seed3

                ( e, seed5 ) =
                    genE seed4
            in
            ( func a b c d e, seed5 )


{-| -}
andMap : Generator s a -> Generator s (a -> b) -> Generator s b
andMap =
    map2 (|>)


{-| -}
andThen : (a -> Generator s b) -> Generator s a -> Generator s b
andThen callback (Generator generateA) =
    Generator <|
        \seed ->
            let
                ( result, newSeed ) =
                    generateA seed

                (Generator generateB) =
                    callback result
            in
            generateB newSeed


{-| -}
filter : (a -> Bool) -> Generator s a -> Generator s a
filter predicate gen =
    Generator (retry gen predicate)


retry : Generator s a -> (a -> Bool) -> s -> ( a, s )
retry gen predicate seed =
    let
        ( candidate, newSeed ) =
            step gen seed
    in
    if predicate candidate then
        ( candidate, newSeed )

    else
        retry gen predicate newSeed


{-| -}
oneIn : Config s -> Int -> Generator s Bool
oneIn c n =
    map ((==) 1) (int c 1 n)


{-| -}
sample : Config s -> List a -> Generator s (Maybe a)
sample c =
    let
        find k ys =
            case ys of
                [] ->
                    Nothing

                z :: zs ->
                    if k == 0 then
                        Just z

                    else
                        find (k - 1) zs
    in
    \xs -> map (\i -> find i xs) (int c 0 (List.length xs - 1))


{-| -}
choice : Config s -> a -> a -> Generator s a
choice c x y =
    map
        (\b ->
            if b then
                x

            else
                y
        )
        (bool c)


{-| -}
choices : Config s -> Generator s a -> List (Generator s a) -> Generator s a
choices c first gens =
    frequency c ( 1, first ) <| List.map (\g -> ( 1, g )) gens


{-| -}
frequency : Config s -> ( Float, Generator s a ) -> List ( Float, Generator s a ) -> Generator s a
frequency config head pairs =
    let
        total =
            List.sum <| List.map (abs << Tuple.first) (head :: pairs)

        pick someChoices n =
            case someChoices of
                ( k, g ) :: rest ->
                    if n <= k then
                        g

                    else
                        pick rest (n - k)

                -- this should never happen
                _ ->
                    Tuple.second head
    in
    float config 0 total |> andThen (pick (head :: pairs))


{-| -}
maybe : Generator s Bool -> Generator s a -> Generator s (Maybe a)
maybe genBool genA =
    genBool
        |> andThen
            (\b ->
                if b then
                    map Just genA

                else
                    constant Nothing
            )
