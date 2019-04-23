module Bytes.Floating.Encode exposing (float16)

{-| Extra Floating-point binary representation for Elm

@docs float16

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E


{-| Encode 16-bit floating point numbers in two bytes.
-}
float16 : Endianness -> Float -> E.Encoder
float16 endian =
    toUnsignedInt32 >> floatToHalf >> E.unsignedInt16 endian



{-------------------------------------------------------------------------------
                                   Internals
-------------------------------------------------------------------------------}


{-| Internal, explicit bit-rounding passing
-}
type Rounding
    = NoRounding
    | RoundUp


{-| Convert a float32 representation to a float16 representation by
shifting bits accordingly within the representation.

               *----- integer part, in base 2
    sign -- *  |
            |  |          *----- decimal part, in base 2
            |  |          |
            | /---\ /-------------\
           (s.iiiii.ddddddddddddddd)2 ~ (-1) ^ s * 1.mmmmmmmmmm * 2^e
                                        \--------/     |
                                            |          *----- exponent
                                            |
                                            *----- mantissa

-}
floatToHalf : Int -> Int
floatToHalf x =
    let
        s =
            x |> shiftRightBy 16 |> and 0x8000

        e =
            x |> shiftRightBy 23 |> and 0xFF

        m =
            x |> and 0x007FFFFF
    in
    case e of
        -- Subnormal underflow
        0 ->
            iEEE754 { s = s, e = 0, m = 0 } NoRounding

        -- +/- Infinity or NaN
        255 ->
            let
                mNext =
                    if m == 0 then
                        0

                    else
                        0x01
            in
            iEEE754 { s = s, e = 31, m = mNext } NoRounding

        _ ->
            let
                eNext =
                    e - 127 + 15
            in
            -- Overflow
            if eNext >= 31 then
                iEEE754 { s = s, e = 31, m = 0 } NoRounding

            else if eNext <= 0 then
                -- Subnormal
                if eNext >= -10 && eNext <= 0 then
                    -- Can represent
                    let
                        r =
                            case m |> or 0x00800000 |> shiftRightBy (13 - eNext) |> and 1 of
                                1 ->
                                    RoundUp

                                _ ->
                                    NoRounding

                        mNext =
                            m |> or 0x00800000 |> shiftRightBy (14 - eNext)
                    in
                    iEEE754 { s = s, e = 0, m = mNext } r

                else
                    -- Subnormal underflow
                    iEEE754 { s = s, e = 0, m = 0 } NoRounding

            else
                -- Normal numbers
                let
                    mNext =
                        m |> shiftRightBy 13

                    r =
                        case m |> shiftRightBy 12 |> and 1 of
                            1 ->
                                RoundUp

                            _ ->
                                NoRounding
                in
                iEEE754 { s = s, e = eNext, m = mNext } r


{-| Leverage existing float32 encoder to _cast_ a float into an unsigned
int on 32 bytes.
-}
toUnsignedInt32 : Float -> Int
toUnsignedInt32 f =
    f
        |> (E.float32 BE >> E.encode)
        |> D.decode (D.unsignedInt32 BE)
        |> Maybe.withDefault (0 // 0)


{-| Recompose a IEEE754 encoding with sign, exponent and mantissa into
a single number. Note that, if rounding is needed, we leverage the
classic sum (+) which may overflow to either an exponent above or,
Infinity which is fine in both cases.
-}
iEEE754 : { s : Int, e : Int, m : Int } -> Rounding -> Int
iEEE754 { s, e, m } rounding =
    let
        r =
            case rounding of
                NoRounding ->
                    0

                RoundUp ->
                    1
    in
    r + (s |> or (e |> shiftLeftBy 10) |> or m)
