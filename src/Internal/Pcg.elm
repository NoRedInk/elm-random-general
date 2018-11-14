module Internal.Pcg exposing (Generator, Seed(..), config, float, fromJson, initialSeed, int, next, peel, toJson)

import Bitwise
import Json.Decode
import Json.Encode
import Random.General as RNG


type Seed
    = Seed Int Int


type alias Generator a =
    RNG.Generator Seed a


int : Int -> Int -> Generator Int
int =
    RNG.int config


float : Float -> Float -> Generator Float
float =
    RNG.float config


initialSeed : Int -> Seed
initialSeed x =
    let
        (Seed state1 incr) =
            -- The magic constant is from Numerical Recipes and is inlined for perf.
            next (Seed 0 1013904223)

        state2 =
            state1 + x |> Bitwise.shiftRightZfBy 0
    in
    next (Seed state2 incr)


next : Seed -> Seed
next (Seed state0 incr) =
    -- The magic constant is from Numerical Recipes and is inlined for perf.
    Seed ((state0 * 1664525) + incr |> Bitwise.shiftRightZfBy 0) incr


toJson : Seed -> Json.Encode.Value
toJson (Seed state incr) =
    Json.Encode.list Json.Encode.int [ state, incr ]


fromJson : Json.Decode.Decoder Seed
fromJson =
    Json.Decode.oneOf
        [ Json.Decode.map2 Seed
            (Json.Decode.index 0 Json.Decode.int)
            (Json.Decode.index 1 Json.Decode.int)
        , Json.Decode.map initialSeed Json.Decode.int
        ]



-- obtain a psuedorandom 32-bit integer


peel : Seed -> Int
peel (Seed state _) =
    -- This is the RXS-M-SH version of PCG, see section 6.3.4 of the paper
    -- and line 184 of pcg_variants.h in the 0.94 C implementation
    let
        word =
            ((state |> Bitwise.shiftRightZfBy ((state |> Bitwise.shiftRightZfBy 28) + 4)) |> Bitwise.xor state) * 277803737
    in
    Bitwise.xor (word |> Bitwise.shiftRightZfBy 22) word
        |> Bitwise.shiftRightZfBy 0


config : RNG.Config Seed
config =
    RNG.makeConfig next peel
