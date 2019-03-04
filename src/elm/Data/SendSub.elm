module Data.SendSub exposing (..)

import Json.Encode as E

type alias PairSub =
    { exchange : String
    , symbol : String
    , userId : String
    , pairId : String
    }


encode : PairSub -> E.Value
encode d =
    E.object
        [ ( "exchange", E.string d.exchange )
        , ( "pair", E.string d.symbol )
        , ( "pairId", E.string d.pairId )
        , ( "userId", E.string d.userId )
        ]