module Bytes.Floating.EncodeTests exposing (suite)

import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Floating.Encode exposing (float16)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bytes.Floating.Encode"
        [ float16 LE 82.125
            |> expect [ 0x22, 0x55 ]
        , float16 BE 12.375
            |> expect [ 0x4A, 0x30 ]
        , float16 BE -0.0
            |> expect [ 0x80, 0x00 ]
        , float16 BE 1.0
            |> expect [ 0x3C, 0x00 ]
        , float16 BE 1.5
            |> expect [ 0x3E, 0x00 ]
        , float16 BE -0.25
            |> expect [ 0xB4, 0x00 ]
        , float16 BE 0.375
            |> expect [ 0x36, 0x00 ]
        , float16 BE 1.001
            |> expect [ 0x3C, 0x01 ]
        , float16 BE 0.99951
            |> expect [ 0x3B, 0xFF ]
        , float16 BE 65504
            |> expect [ 0x7B, 0xFF ]
        , float16 BE 0.000061035
            |> expect [ 0x04, 0x00 ]
        , float16 BE 0.000000059605
            |> expect [ 0x00, 0x01 ]
        , float16 BE 0.000000001
            |> expect [ 0x00, 0x00 ]
        , float16 BE 12547414
            |> expect [ 0x7C, 0x00 ]
        , float16 BE (-1 / 0)
            |> expect [ 0xFC, 0x00 ]
        , float16 BE (0 / 0)
            |> expectOneOf [ [ 0xFC, 0x01 ], [ 0x7C, 0x01 ] ]
        ]


{-| Alias / Shortcut to write test cases
-}
expect : List Int -> E.Encoder -> Test
expect output input =
    test (Debug.toString input ++ " -> " ++ Debug.toString output) <|
        \_ -> hex (E.encode input) |> Expect.equal (Just output)


expectOneOf : List (List Int) -> E.Encoder -> Test
expectOneOf outputs input =
    test (Debug.toString input ++ " -> one of " ++ Debug.toString outputs) <|
        \_ ->
            Expect.true
                "expected one of given outputs"
                (List.any (\x -> Just x == hex (E.encode input)) outputs)


{-| Convert a list of BE unsigned8 to bytes
-}
hex : Bytes -> Maybe (List Int)
hex bytes =
    bytes
        |> D.decode
            (D.loop ( width bytes, [] )
                (\( n, xs ) ->
                    if n == 0 then
                        xs |> List.reverse |> D.Done |> D.succeed

                    else
                        D.unsignedInt8
                            |> D.map (\x -> D.Loop ( n - 1, x :: xs ))
                )
            )
