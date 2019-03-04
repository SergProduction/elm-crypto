module Data.TakePair exposing
    ( Pair
    , decode
    )

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Dict


type alias A =
    { current : String
    , prev : String
    }


type alias B =
    { sell : String
    , buy : String
    }


type alias C =
    { currentDay : String
    , prevDay : String
    , twoPrevDay : String
    }

type alias Graph =
  { x : Float
  , y : Float
  }

type alias Pair =
    { exchange : String
    , symbol : String
    , priceDiff : String
    , highVolume24 : String
    , time : String
    , bid : A
    , ask : A
    , low : A
    , high : A
    , volume24 : A
    , marketCup : A
    , marketVol24 : A
    , marketHistory : B
    , interestRatioNow : B
    , baseVolume : C
    , graph : List Graph
    }


decode : D.Decoder Pair
decode =
    D.succeed Pair
        |> required "Exchange" D.string
        |> required "Symbol" D.string
        |> required "PriceDiff" D.string
        |> required "HighVolume24" D.string
        |> required "Time" D.string
        |> required "Bid" decodeA
        |> required "Ask" decodeA
        |> required "Low" decodeA
        |> required "High" decodeA
        |> required "Volume24" decodeA
        |> required "MarketCup" decodeA
        |> required "MarketVol24" decodeA
        |> required "MarketHistory" decodeB
        |> required "InterestRatioNow" decodeB
        |> required "BaseVolume" decodeC
        |> required "Graphics" (D.list decodeGraph)


decodeA : D.Decoder A
decodeA =
    D.map2 A
        (D.field "current" D.string)
        (D.field "prev" D.string)


decodeB : D.Decoder B
decodeB =
    D.map2 B
        (D.field "sell" D.string)
        (D.field "buy" D.string)


decodeC : D.Decoder C
decodeC =
    D.map3 C
        (D.field "currentDay" D.string)
        (D.field "prevDay" D.string)
        (D.field "twoPrevDay" D.string)

decodeGraph : D.Decoder Graph
decodeGraph =
    D.map2 Graph
        (D.field "x" D.float)
        (D.field "y" D.float)

