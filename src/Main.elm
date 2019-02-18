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
import View.Tile exposing (viewTileList)


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


type Msg
    = Echo (Result D.Error Data)
    | ToggleModal
    | View ViewType
    | AddPair
    | User User.Msg
    | Test (Result Http.Error String)


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
                viewTileList model.data
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
            [ button [ onClick (View Table) ] [ text "table" ]
            , button [ onClick (View Squart) ] [ text "tile" ]
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
