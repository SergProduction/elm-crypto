module View.Tile exposing (viewTileList)

import Data.Pair as DataModule exposing (Pair)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import View.Helpers exposing (getRedOrGreenClass)


viewTileList : (Pair -> msg) -> Dict.Dict String Pair -> Html msg
viewTileList msg model =
    div [ class "tile-list" ] (List.map (viewTile msg) (Dict.values model))


viewTile : (Pair -> msg) -> Pair -> Html msg
viewTile msg d =
    div [ class "tile" ]
        [ div [ class "flex-row flex-between" ]
            [ span [ class "name burse" ] [ text d.exchange ]
            , span [ class "name" ] [ text d.symbol ]
            , span [ class "red" ] [ text "$2222.00002" ]
            , span [ class "burse time" ] [ text d.time ]
            , div [ class "table-row-menu" ]
                [ button [ class "btn transparent", onClick (msg d) ]
                    [ i [ class "menu fas fa-ellipsis-h" ] [] ]
                ]
            ]
        , div [ class "flex-row flex-between" ]
            [ div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "BID" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.bid.prev d.bid.current ] ]
                    [ text d.bid.current ]
                , div [ class "name" ] [ text "ASK" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.ask.prev d.ask.current ] ]
                    [ text d.ask.current ]
                ]
            , div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "24h High" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.high.prev d.high.current ] ]
                    [ text d.high.current ]
                , div [ class "name" ] [ text "24h Low" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.low.prev d.low.current ] ]
                    [ text d.low.current ]
                ]
            , div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "24h Vol" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.baseVolume.prevDay d.baseVolume.currentDay ] ]
                    [ text d.baseVolume.currentDay ]
                , div [ class "name" ] [ text "Pre Vol" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.baseVolume.twoPrevDay d.baseVolume.prevDay ] ]
                    [ text d.baseVolume.prevDay ]
                ]
            ]
        , div [ class "flex-row flex-between" ]
            [ div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "Coin Volume 24h" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                    [ text d.volume24.current ]
                , div [ class "name" ] [ text "Coin Volume 24h High" ]
                , div [ classList [ ( "value", True ), ( "gt", True ) ] ]
                    [ text d.highVolume24 ]
                ]
            , div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "MH 24h Buy" ]
                , div [ class "value" ] [ text d.marketHistory.buy ]
                , div [ class "name" ] [ text "MH 24h Sell" ]
                , div [ class "value" ] [ text d.marketHistory.sell ]
                ]
            ]
        , div [ class "flex-row flex-between" ]
            [ div []
                [ div [ class "name" ] [ text "Interest Ratio Now" ]
                , div [ class "flex-row interest-ratio" ]
                    [ div [ class "flex-column" ]
                        [ span [ class "interest-ratio-value green" ] [ text (d.interestRatioNow.buy ++ "%") ]
                        , span [] [ text "IRN Buy" ]
                        ]
                    , div [ class "flex-column" ]
                        [ span [ class "interest-ratio-value red" ] [ text (d.interestRatioNow.sell ++ "%") ]
                        , span [] [ text "IRN Sell" ]
                        ]
                    ]
                ]
            , div []
                [ div [ class "flex-row flex-between"]
                    [ div [ class "mango"] [text "Price 24h"]
                    , div [] [text d.priceDiff]
                    ]
                , div [] [text "GRAPH"]
                ]
            ]
        , div [ class "flex-row flex-between" ]
            [ div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "Market Cap" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                    [ text d.marketCup.current ]
                ]
            , div [ class "name-value-group" ]
                [ div [ class "name" ] [ text "24 Market Vol" ]
                , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                    [ text d.marketVol24.current ]
                ]
            ]
        ]

