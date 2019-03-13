module MainHelpers exposing (MoveData(..), shiftData)

import Data.TakePair as TakePair
import Dict
import Array

type MoveData
  = Top
  | Up
  | Down




shiftData : (Dict.Dict String Int, Array.Array TakePair.Pair) -> TakePair.Pair -> MoveData -> Maybe (Dict.Dict String Int, Array.Array TakePair.Pair)
shiftData (dataMap, data) pair move =
    case move of
      Top ->
        case Dict.get pair.id dataMap of
          Nothing -> Nothing
          Just index ->
            case Array.get index data of
              Nothing -> Nothing
              Just _ ->
                let
                  arrWithoutPair =
                    Array.filter (\p -> not (p.id == pair.id)) data

                  arrWithFirstPair =
                    Array.append ( Array.fromList [pair] ) arrWithoutPair
                in
                  Just ( Dict.fromList <| List.map (\(idx, it) -> (it.id, idx)) <| Array.toIndexedList arrWithFirstPair
                       , arrWithFirstPair
                       )

      _ ->
        let
          toIndex =
            case move of
              Up -> (\i -> i - 1)

              Down -> ((+)1)

              _ -> ((+)1) -- WTF
                    
        in
          case Dict.get pair.id dataMap of
              Nothing ->
                Nothing

              Just index ->
                let
                  fromPair =
                    Array.get index data

                  toPair =
                    Array.get (toIndex index) data

                  resultDataMap =
                    Maybe.map2
                      (\from to ->
                        Dict.insert to.id index <| Dict.insert from.id (toIndex index) dataMap
                      )
                      fromPair
                      toPair

                  resultArray =
                    Maybe.map2
                      (\from to ->
                        Array.set index to <| Array.set (toIndex index) from data
                      )
                      fromPair
                      toPair

                  modelUpdate =
                    Maybe.map2
                      (\dict arr ->
                        ( dict, arr )
                      )
                      resultDataMap
                      resultArray
                in
                  case modelUpdate of
                    Just newModel ->
                      Just newModel

                    Nothing ->
                      Nothing