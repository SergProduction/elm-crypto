port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Data as DataModule exposing (Data, decode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import User


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { data : List Data
    , modal : Bool
    , user : User.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = []
      , modal = True
      , user = User.init
      }
    , Cmd.none
    )


type Msg
    = Echo (Result D.Error Data)
    | ToggleModal
    | User User.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleModal ->
            ( { model | modal = not model.modal }, Cmd.none )

        User userMsg ->
            case User.update userMsg model.user of
                ( user, command ) ->
                    ( { model | user = user }, Cmd.map User command )

        Echo result ->
            case result of
                Ok d ->
                    ( { model | data = d :: model.data }, Cmd.none )

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
        , button [ onClick ToggleModal ]
            [ text "Sign-in" ]
        , viewModal model
        , viewTable model
        ]


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        False ->
            text ""

        True ->
            div [ class "modal flex-row flex-center" ]
                [ div [ class "flex-column modal-form" ]
                    [ div [ class "close-modal", onClick ToggleModal ] [ text "close" ]
                    , User.view model.user |> Html.map User
                    ]
                ]


viewTable : Model -> Html Msg
viewTable model =
    div [ class "flex-row flex-center" ]
        [ table []
            [ tbody [] (List.map viewCeil model.data)
            ]
        ]


viewCeil : Data -> Html Msg
viewCeil d =
    tr []
        [ td []
            [ div [ class "name" ] [ text d.exchange ]
            , div [ class "value" ] [ text "ET TIME" ]
            , div [ class "name" ] [ text d.symbol ]
            ]
        , td []
            [ div [ class "name" ] [ text "BID" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.bid.prev d.bid.current ] ]
                [ text d.bid.current ]
            , div [ class "name" ] [ text "ASK" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.ask.prev d.ask.current ] ]
                [ text d.ask.current ]
            ]
        , td []
            [ div [ class "name" ] [ text "24h High" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.high.prev d.high.current ] ]
                [ text d.high.current ]
            , div [ class "name" ] [ text "24h Low" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.low.prev d.low.current ] ]
                [ text d.low.current ]
            ]
        , td []
            [ div [ class "name" ] [ text "Coin Volume 24h" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.volume24.prev d.volume24.current ] ]
                [ text d.volume24.current ]
            , div [ class "name" ] [ text "Coin Volume 24h High" ]
            , div [ classList [ ( "value", True ), ( "gt", True ) ] ]
                [ text d.highVolume24 ]
            ]
        , td []
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
        , td []
            [ div [ class "name" ] [ text "24h Vol" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.baseVolume.prevDay d.baseVolume.currentDay ] ]
                [ text d.baseVolume.currentDay ]
            , div [ class "name" ] [ text "Pre Vol" ]
            , div [ classList [ ( "value", True ), getRedOrGreenClass d.baseVolume.twoPrevDay d.baseVolume.prevDay ] ]
                [ text d.baseVolume.prevDay ]
            ]
        , td []
            [ div [ class "name" ] [ text "Market Cup" ]
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
