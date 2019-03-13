module View.Graph exposing (viewGraph)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


svgWidth = 110

svgHeight = 30


type alias Point =
  { x : Float
  , y : Float
  }

listToPath : List Point -> String
listToPath data =
    let
      ds = List.foldl
            (\d acc ->
              acc ++ (String.fromFloat (d.x * svgWidth)) ++ " " ++ (String.fromFloat (   (d.y * svgHeight) * -1 + svgHeight )) ++ ", "
            )
            ""
            data
    in
      String.slice 0 -2 ds


viewGraph : List Point -> Html msg
viewGraph graphData =
  svg
    [ width <| String.fromInt svgWidth
    , height <| String.fromInt svgHeight
    , viewBox "0 0 110 30"
    ]
    [ polyline
      [ fill "none"
      , stroke "#0099ff" -- (getRedOrGreen graphData)
      , strokeWidth "2"
      , points <| listToPath graphData
      ] []  
    ]

getRedOrGreen : List Point -> String
getRedOrGreen graphData =
  let
      rev = List.reverse graphData
  in
    let
        maybeLast = List.head rev
        maybePrevLast = List.head <| List.take ( (List.length graphData) - 2) graphData
    in
      let
        maybeColor =
          Maybe.map2
            (\prevLast last ->
              if prevLast.y > last.y then
                "#008951"
    
              else if prevLast.y < last.y then
                "#ff0002"
          
              else
                "#0099ff"
            )
            maybeLast
            maybePrevLast
      in
        case maybeColor of
          Nothing -> ""
          Just color -> color