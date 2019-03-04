module Data.SendUnSub exposing (..)

import Json.Encode as E


type alias PairUnSub =
    { exchange : String
    , pair : String
    , userId : String
    }



encode : PairUnSub -> E.Value
encode d =
    E.object
        [ ( "exchange", E.string d.exchange )
        , ( "pair", E.string d.pair )
        , ( "userId", E.string d.userId )
        ]