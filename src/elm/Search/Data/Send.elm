module Search.Data.Send exposing (..)

import Json.Encode as E


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