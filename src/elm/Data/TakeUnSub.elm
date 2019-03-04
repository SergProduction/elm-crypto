module Data.TakeUnSub exposing (..)

import Json.Decode as D


type alias PairUnSub =
    { message : Bool
    , pairId : String
    }


dencode : D.Decoder PairUnSub
dencode =
    D.map2 PairUnSub
        (D.field "message" D.bool)
        (D.field "pairId" D.string)

