module View.Table exposing (viewTable)

import Data.Pair as DataModule exposing (Pair)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import View.Helpers exposing (getRedOrGreenClass)


viewTable : (Pair -> msg) -> Dict.Dict String Pair -> Html msg
viewTable msg model =
    if (Dict.isEmpty model) then
        viewPreloader
    else
      div [ class "flex-row flex-center" ]
          [ table []
              [ tbody [] (List.map (viewRow msg) (Dict.values model))
              ]
          ]


viewRow : (Pair -> msg) -> Pair -> Html msg
viewRow msg d =
    tr []
        [ td [ class "name-value-group" ]
            [ div [ class "name burse" ] [ text d.exchange ]
            , div [ class "burse time" ] [ text d.time ]
            , div [ class "name" ] [ text d.symbol ]
            ]
        , td [ class "name-value-group" ]
            [ div [ class "name" ] [ text "BID" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.bid.prev d.bid.current ] ]
                [ text d.bid.current ]
            , div [ class "name" ] [ text "ASK" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.ask.prev d.ask.current ] ]
                [ text d.ask.current ]
            ]
        , td [ class "name-value-group" ]
            [ div [ class "name" ] [ text "24h High" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.high.prev d.high.current ] ]
                [ text d.high.current ]
            , div [ class "name" ] [ text "24h Low" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.low.prev d.low.current ] ]
                [ text d.low.current ]
            ]
        , td [ class "name-value-group" ]
            [ div [ class "name" ] [ text "Coin Volume 24h" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                [ text d.volume24.current ]
            , div [ class "name" ] [ text "Coin Volume 24h High" ]
            , div [ classList [ ( "value", True ), ( "gt", True ) ] ]
                [ text d.highVolume24 ]
            ]
        , td [ class "name-value-group" ]
            [ div [ class "name" ] [ text "MH 24h Buy" ]
            , div [ class "value" ] [ text d.marketHistory.buy ]
            , div [ class "name" ] [ text "MH 24h Sell" ]
            , div [ class "value" ] [ text d.marketHistory.sell ]
            ]
        , td []
            [ div [ class "name" ] [ text "Interest Ratio Now" ]
            , div [ class "flex-row interest-ratio roboto" ]
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
        , td [ class "name-value-group" ]
            [ div [ class "name" ] [ text "24h Vol" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.baseVolume.prevDay d.baseVolume.currentDay ] ]
                [ text d.baseVolume.currentDay ]
            , div [ class "name" ] [ text "Pre Vol" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.baseVolume.twoPrevDay d.baseVolume.prevDay ] ]
                [ text d.baseVolume.prevDay ]
            ]
        , td [ class "name-value-group" ]
            [ div [ class "name" ] [ text "Market Cap" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                [ text d.marketCup.current ]
            , div [ class "name" ] [ text "24 Market Vol" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                [ text d.marketVol24.current ]
            ]
        , td [ class "name-value-group" ]
            [ div [ class "table-row-menu" ]
                [ button [ class "btn transparent", onClick (msg d) ]
                    [ i [ class "menu fas fa-ellipsis-h" ] [] ]
                ]
            , div [] [ text "GRAPH" ]
            , div [ class "flex-row flex-between"]
              [ div [ class "mango"] [text "Price 24h"]
              , div [] [text d.priceDiff]
              ]
            ]
        ]


viewPreloader : Html msg
viewPreloader =
    div [ class "flex-row flex-center" ]
        [ table []
            [ tbody [] (List.map viewRowPreloader (List.repeat 5 0))
            ]
        ]


viewRowPreloader : a -> Html msg
viewRowPreloader _ = 
    tr [] (List.map (\x ->
        td [ class "name-value-group" ]
              [ div [ class "preloader" ] []
              , div [ class "preloader" ] []
              , div [ class "preloader" ] []
              , div [ class "preloader" ] []
              ]
      ) (List.repeat 9 0))