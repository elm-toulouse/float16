<p align="center">
  <img src=".github/logo.png" /><br/>
  <a href="https://package.elm-lang.org/packages/elm-toulouse/float16/latest/"><img src="https://img.shields.io/elm-package/v/elm-toulouse/float16.svg?style=for-the-badge"/></a>
</p>

## Getting Started

### Installation

```
elm install elm-toulouse/float16
```

### Usage

```elm
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Floating.Encode as E

-- ENCODER

encode : Float -> E.Encoder
encode f =
  E.sequence 
    [ E.float16 BE f
    , E.float32 BE f
    , E.float64 BE f
    ]

-- DECODER

decode : D.Decoder (Float, Float, Float)
decode =
  D.map3 (\a b c -> (a, b, c))
    |> D.float16 BE 
    |> D.float32 BE
    |> D.float64 BE
```

## Testing

Code coverage available [here](https://elm-toulouse.github.io/float16)

## Changelog

[CHANGELOG.md](CHANGELOG.md)

---

<p align="center"><img src="https://img.shields.io/github/license/elm-toulouse/float16.svg?style=for-the-badge" /> </p>
