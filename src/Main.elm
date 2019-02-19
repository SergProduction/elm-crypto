port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Data.Pair exposing (Pair, decodePair, defaultSubcribe)
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
    { data : Dict.Dict String Pair
    , modal : Bool
    , viewType : ViewType
    , user : User.Model
    }


type Msg
    = EchoWs (Result D.Error Pair)
    | User User.Msg
    | ToggleModal
    | View ViewType
    | AddPair
    | RespondePairs (Result Http.Error String)
    | DefaultSubcribe


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = Dict.empty
      , viewType = Table
      , modal = False
      , user = User.init
      }
    , Http.get
        { url = "http://142.93.47.26:1023/pairs"
        , expect = Http.expectString RespondePairs
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        User userMsg ->
            let
                ( user, command ) =
                    User.update userMsg model.user
            in
            ( { model | user = user }, Cmd.map User command )

        View t ->
            case t of
                Table ->
                    ( { model | viewType = Table }, Cmd.none )

                Squart ->
                    ( { model | viewType = Squart }, Cmd.none )

        AddPair ->
            ( model, Cmd.none )

        DefaultSubcribe ->
            case model.user.userkey of
                Nothing ->
                    ( model, Cmd.none )

                Just ukey ->
                    ( model, getPair (defaultSubcribe ukey) )

        RespondePairs _ ->
            ( model, Cmd.none )

        ToggleModal ->
            ( { model | modal = not model.modal }, Cmd.none )

        EchoWs result ->
            case result of
                Ok d ->
                    let
                        data =
                            Dict.insert d.interestRatioNow.buy d model.data
                    in
                    ( { model | data = data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


port wsListenPairs : (String -> msg) -> Sub msg


port getPair : E.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    wsListenPairs (\s -> EchoWs (D.decodeString decodePair s))


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
    div [ class "header flex-row flex-between flex-vertical-center roboto" ]
        [ div [ class "logo-name" ] [ text "CDQ Screener" ]
        , div [ class "flex-row" ]
            [ button [ class "red-button", onClick DefaultSubcribe ] [ text "SRH |>" ]
            , button [ class "pair-button", onClick AddPair ] [ text "< ADD PAIR >" ]
            ]
        , div [] [ text "Market Cap: $120 558 456 737 • 24h Vol: $20 850 816 957 • BTC Dominance: 52.7%" ]
        , div [ class "options-view" ]
            [ button [ onClick (View Table) ]
                [ i [ class "disactive fas fa-th-list" ] [] ]
            , button [ onClick (View Squart) ]
                [ i [ class "active fas fa-th-large" ] [] ]
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
                    [ i [ class "fas fa-times", onClick ToggleModal ] []
                    , User.view model.user |> Html.map User
                    ]
                ]
