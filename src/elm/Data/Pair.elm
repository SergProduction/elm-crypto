module Data.Pair exposing
    ( Pair
    , decodePair
    )

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


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
    }


decodePair : D.Decoder Pair
decodePair =
    D.succeed Pair
        |> required "Symbol" D.string
        |> required "Exchange" D.string
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
        -- , ( "userId", E.string d.userId )
        ]
