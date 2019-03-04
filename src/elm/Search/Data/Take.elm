module Search.Data.Take exposing (..)

import Json.Decode as D
import Dict


type alias Pair =
    { exchange : String
    , symbol : String
    , ask : String
    , bid : String
    }

type alias Pairs = List Pair


type alias RowPair =
  { symbol : String
  , ask : String
  , bid : String
  }

type alias RowPairs =
  Dict.Dict String ( List RowPair )


decodeRowPairs : D.Decoder RowPairs
decodeRowPairs =
    D.dict
      <| D.list
        <| D.map3 RowPair
            (D.field "Symbol" D.string)
            (D.field "Ask" D.string)
            (D.field "Bid" D.string)


transformRowPairToPair : RowPairs -> Pairs
transformRowPairToPair rowPairs =
  let
    d =
      List.map
      (\(exchange, v) -> List.map (\rowPair -> Pair exchange rowPair.symbol rowPair.ask rowPair.bid ) v )
      (Dict.toList rowPairs)
  in
    List.concat d