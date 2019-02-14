port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Data as DataModule exposing (Data, decode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    List Data


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], Cmd.none )


type Msg
    = Echo (Result D.Error Data)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Echo result ->
            case result of
                Ok d ->
                    ( d :: model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


port ws : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    ws (\s -> Echo (D.decodeString decode s))


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Begin ws" ]
        , table []
            [ tbody [] (List.map viewCeil model)
            ]
        ]


viewCeil : Data -> Html Msg
viewCeil d =
    tr []
        [ td []
            [ div []
                [ div [ class "name" ] [ text d.symbol ]
                ]
            ]
        , td []
            [ div [ class "name" ] [ text "BID" ]
            , div [ class "value" ] [ text d.bid.current ]
            , div [ class "name" ] [ text "ASK" ]
            , div [ class "value" ] [ text d.ask.current ]
            ]
        , td []
            [ div [ class "name" ] [ text "24h High" ]
            , div [ class "value" ] [ text d.high.current ]
            , div [ class "name" ] [ text "24h Low" ]
            , div [ class "value" ] [ text d.low.current ]
            ]
        , td []
            [ div [ class "name" ] [ text "CV 24h" ]
            , div [ class "value" ] [ text "-" ]
            , div [ class "name" ] [ text "CV 24h High" ]
            , div [ class "value" ] [ text "-" ]
            ]
        , td []
            [ div [ class "name" ] [ text "MH 24h Buy" ]
            , div [ class "value" ] [ text d.marketHistory.buy ]
            , div [ class "name" ] [ text "MH 24h Sell" ]
            , div [ class "value" ] [ text d.marketHistory.sell ]
            ]
        , td []
            [ div [ class "name" ] [ text "Interest Ratio Now" ]
            , div [ class "value" ] [ text "-" ]
            ]
        , td []
            [ div [ class "name" ] [ text "24h Vol" ]
            , div [ class "value" ] [ text "-" ]
            , div [ class "name" ] [ text "Pre Vol" ]
            , div [ class "value" ] [ text "-" ]
            ]
        , td []
            [ div [ class "name" ] [ text "Market Cup" ]
            , div [ class "value" ] [ text d.marketCup.current ]
            , div [ class "name" ] [ text "24 Market Vol" ]
            , div [ class "value" ] [ text d.marketVol24.current ]
            ]
        ]
