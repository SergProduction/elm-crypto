module View.Table exposing (viewTable)

import Data as DataModule exposing (Data)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


viewTable : Dict.Dict String Data -> Html msg
viewTable model =
    div [ class "flex-row flex-center" ]
        [ table []
            [ tbody [] (List.map viewCeil (Dict.values model))
            ]
        ]


viewCeil : Data -> Html msg
viewCeil d =
    tr []
        [ td [ class "name-value-group" ]
            [ div [ class "name" ] [ text d.exchange ]
            , div [ class "value" ] [ text "ET TIME" ]
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
            , div [ class "flex-row interest-ratio" ]
                [ div [ class "flex-column" ]
                    [ span [ class "interest-ratio-value lt" ] [ text (d.interestRatioNow.buy ++ "%") ]
                    , span [] [ text "IRN Buy" ]
                    ]
                , div [ class "flex-column" ]
                    [ span [ class "interest-ratio-value gt" ] [ text (d.interestRatioNow.sell ++ "%") ]
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
        ]


getRedOrGreenClass : String -> String -> ( String, Bool )
getRedOrGreenClass prev next =
    case String.toFloat prev of
        Just p ->
            case String.toFloat next of
                Just n ->
                    if p > n then
                        ( "lt", True )

                    else if p < n then
                        ( "gt", True )

                    else
                        ( "", False )

                Nothing ->
                    ( "", False )

        Nothing ->
            ( "", False )
