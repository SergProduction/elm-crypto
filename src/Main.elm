port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Data as DataModule exposing (Data, decode)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import User
import View.Table exposing (viewTable)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type ViewType
    = Table
    | Squart


type alias Model =
    { data : Dict.Dict String Data
    , modal : Bool
    , viewType : ViewType
    , user : User.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = Dict.empty
      , viewType = Squart
      , modal = False
      , user = User.init
      }
    , Http.get
        { url = "http://142.93.47.26:1023/pairs"
        , expect = Http.expectString Test
        }
    )


type Msg
    = Echo (Result D.Error Data)
    | ToggleModal
    | View ViewType
    | AddPair
    | User User.Msg
    | Test (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        View t ->
            case t of
                Table ->
                    ( { model | viewType = Table }, Cmd.none )

                Squart ->
                    ( { model | viewType = Squart }, Cmd.none )

        AddPair ->
            ( model, getPair """{ userId = "12345",exchange = " binance",pair = " BTCETH", Line =" 10" }""" )

        Test _ ->
            ( model, Cmd.none )

        ToggleModal ->
            ( { model | modal = not model.modal }, Cmd.none )

        User userMsg ->
            let
                ( user, command ) =
                    User.update userMsg model.user
            in
            ( { model | user = user }, Cmd.map User command )

        Echo result ->
            case result of
                Ok d ->
                    let
                        data =
                            Dict.insert d.interestRatioNow.buy d model.data
                    in
                    ( { model | data = data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


port ws : (String -> msg) -> Sub msg


port getPair : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    ws (\s -> Echo (D.decodeString decode s))


view : Model -> Html Msg
view model =
    div []
        [ viewHead model
        , viewModal model
        , case model.viewType of
            Table ->
                viewTable model.data

            Squart ->
                viewSquartList model.data
        ]


viewHead : Model -> Html Msg
viewHead model =
    div [ class "flex-row flex-between flex-vertical-center" ]
        [ div [ class "logo-name" ] [ text "CDQ Screener" ]
        , div [ class "flex-row" ]
            [ button [ class "red-button" ] [ text "SRH |>" ]
            , button [ class "pair-button", onClick AddPair ] [ text "< ADD PAIR >" ]
            ]
        , div [] [ text "Market Cap: $120 558 456 737 • 24h Vol: $20 850 816 957 • BTC Dominance: 52.7%" ]
        , div [ class "options-view" ]
            [ span [ onClick (View Table) ]
                [ text "table" ]
            , span [ onClick (View Squart) ]
                [ text "tile" ]
            ]
        , case model.user.userkey of
            Nothing ->
                button [ class "sign-in-button", onClick ToggleModal ] [ text "Sign-in" ]

            Just _ ->
                span [] [ text model.user.email ]
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


viewSquartList : Dict.Dict String Data -> Html Msg
viewSquartList model =
    div [ class "tile-list" ] (List.map viewSquart (Dict.values model))


viewSquart : Data -> Html Msg
viewSquart d =
    div [ class "tile" ]
        [ div [ class "flex-row flex-between" ]
            [ span [ class "name" ] [ text d.exchange ]
            , span [ class "name" ] [ text d.symbol ]
            , span [] [ text "$2222.00002" ]
            , span [] [ text "ET TIME" ]
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
                        [ span [ class "interest-ratio-value lt" ] [ text (d.interestRatioNow.buy ++ "%") ]
                        , span [] [ text "IRN Buy" ]
                        ]
                    , div [ class "flex-column" ]
                        [ span [ class "interest-ratio-value gt" ] [ text (d.interestRatioNow.sell ++ "%") ]
                        , span [] [ text "IRN Sell" ]
                        ]
                    ]
                ]
            , div []
                [ text "GRAPH"
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
