module Data.Pair exposing
    ( Pair
    , decodePair
    , defaultSubcribe
    )

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E



{-
   {
     "Symbol":"ETHBTC",
     "Exchange":"BINANCE",
     "HighVolume24":"164625.909",
     "Bid":{"current":"0.033634","prev":"0.033671"},
     "Ask":{"current":"0.033643","prev":"0.033672"},
     "Low":{"current":"0.033531","prev":"0.033531"},
     "High":{"current":"0.034371","prev":"0.034371"},
     "Volume24":{"current":"159306.81","prev":"161977.811"},
     "MarketCup":{"current":"3.31153248592E9","prev":"0.0"},
     "MarketVol24":{"current":"1.2787931053E10","prev":"0.0"}
     "MarketHistory":{"sell":"26700.74200000019","buy":"26006.495999999803"},
     "InterestRatioNow":{"sell":"96","buy":"4"},
     "BaseVolume":{"currentDay":"5397.80119079","prevDay":"5489.05795146","twoPrevDay":"0.0"},
   }
-}
{-
   defaultpair

   BTCNEM
   BTCXVG
   BTCDGB
   BTCKMD
   BTCDASH
   BTCNAV

   default berge
   BITTREX
-}


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


type alias Pair =
    { symbol : String
    , exchange : String
    , highVolume24 : String
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
    }


decodePair : D.Decoder Pair
decodePair =
    D.succeed Pair
        |> required "Symbol" D.string
        |> required "Exchange" D.string
        |> required "HighVolume24" D.string
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


type alias SubcribePair =
    { exchange : String
    , pair : String
    , line : String
    , userId : String
    }


encodeSubcribePair : SubcribePair -> E.Value
encodeSubcribePair d =
    E.object
        [ ( "exchange", E.string d.exchange )
        , ( "pair", E.string d.pair )
        , ( "line", E.string d.line )
        , ( "userId", E.string d.userId )
        ]


defaultSubcribe ukey =
    encodeSubcribePair
        { exchange = "BITTREX"
        , pair = "BTCNEM"
        , line = "0"
        , userId = ukey
        }
