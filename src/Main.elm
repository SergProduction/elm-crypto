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
import Search exposing (JSONPairSymbols, PairSymbolsList, decodeJSONPairSymbols, transformJSONToPairSymbolsList)
import User
import View.Table exposing (viewTable)
import View.Tile exposing (viewTileList)


type alias PairSub =
    { exchange : String
    , pair : String
    , userId : String
    , pairId : String
    }


type alias PairUnSub =
    { exchange : String
    , pair : String
    , pairId : String
    }


encodeSubcribePair : PairSub -> E.Value
encodeSubcribePair d =
    E.object
        [ ( "exchange", E.string d.exchange )
        , ( "pair", E.string d.pair )
        , ( "pairId", E.string d.pairId )
        , ( "userId", E.string d.userId )
        ]


encodeUnSubcribePair : PairUnSub -> E.Value
encodeUnSubcribePair d =
    E.object
        [ ( "exchange", E.string d.exchange )
        , ( "pair", E.string d.pair )
        , ( "pairId", E.string d.pairId )
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = EchoWs (Result D.Error Pair)
    | User User.Msg
    | Search Search.Msg
    | View ViewType
    | ToggleModalSignIn
    | UnSubPair Pair


type ViewType
    = Table
    | Squart
    | SearchView


type alias Model =
    { data : Dict.Dict String Pair
    , modal : Bool
    , viewType : ViewType
    , prevViewType : ViewType
    , isFind : Bool
    , user : User.Model
    , search : Search.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = Dict.empty
      , viewType = Table
      , prevViewType = Table
      , isFind = False
      , modal = False
      , user = User.init
      , search = Search.init
      }
    , Cmd.map Search Search.getPairSymbols
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        User (User.ResponseSuccess ukey) ->
            let
                ( userModel, command ) =
                    User.update (User.ResponseSuccess ukey) model.user
            in
            ( { model | modal = False, user = userModel }, Cmd.none )

        User userMsg ->
            let
                ( userModel, command ) =
                    User.update userMsg model.user
            in
            ( { model | user = userModel }, Cmd.map User command )

        Search (Search.SubscribePair p) ->
            case model.user.userId of
                Nothing ->
                    ( model, Cmd.none )

                Just userId ->
                    ( model
                    , toJs <|
                        encodeSubcribePair <|
                            PairSub (String.toUpper p.exchange) p.pair userId (p.exchange ++ p.pair)
                    )

        Search searchMsg ->
            let
                ( search, command ) =
                    Search.update searchMsg model.search
            in
            ( { model | search = search }, Cmd.map Search command )

        UnSubPair pair ->
            let
                pairUnSub =
                    PairUnSub (String.toUpper pair.exchange) pair.symbol (pair.exchange ++ pair.symbol)
            in
            ( model, toJs (encodeUnSubcribePair pairUnSub) )

        View t ->
            case t of
                Table ->
                    ( { model | viewType = Table }, Cmd.none )

                Squart ->
                    ( { model | viewType = Squart }, Cmd.none )

                SearchView ->
                    case model.user.userId of
                        Nothing ->
                            ( { model | modal = True }, Cmd.none )

                        Just _ ->
                            if model.isFind then
                                ( { model
                                    | isFind = False
                                    , viewType = model.prevViewType
                                  }
                                , Cmd.none
                                )

                            else
                                ( { model
                                    | isFind = True
                                    , viewType = SearchView
                                    , prevViewType = model.viewType
                                  }
                                , Cmd.none
                                )

        ToggleModalSignIn ->
            ( { model | modal = not model.modal }, Cmd.none )

        EchoWs result ->
            case result of
                Ok d ->
                    let
                        data =
                            Dict.insert (d.exchange ++ d.symbol) d model.data
                    in
                    ( { model | data = data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


port wsListenPairs : (String -> msg) -> Sub msg


port toJs : E.Value -> Cmd msg


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
                viewTable UnSubPair model.data

            Squart ->
                viewTileList UnSubPair model.data

            SearchView ->
                Search.view model.search |> Html.map Search
        ]


viewHead : Model -> Html Msg
viewHead model =
    div [ class "header flex-row flex-between flex-vertical-center roboto" ]
        [ div [ class "logo-name" ] [ text "CDQ Screener" ]
        , div [ class "flex-row" ]
            [ button [ class "red-button" ] [ text "SRH |>" ]
            , if model.isFind then
                Search.viewSearchInput model.search |> Html.map Search

              else
                text ""
            , if not model.isFind then
                button [ class "pair-button", onClick (View SearchView) ] [ text "< ADD PAIR >" ]

              else
                text ""
            ]
        , div [] [ text "Market Cap: $120 558 456 737 • 24h Vol: $20 850 816 957 • BTC Dominance: 52.7%" ]
        , div [ class "options-view flex-row" ]
            [ button [ onClick (View Table) ]
                [ i [ classList [ ( "fas fa-th-list", True ), isActive Table model.viewType ] ] [] ]
            , button [ onClick (View Squart) ]
                [ i [ classList [ ( "fas fa-th-large", True ), isActive Squart model.viewType ] ] [] ]
            ]
        , if model.isFind then
            i [ class "fas fa-times", onClick (View SearchView) ] []

          else
            case model.user.userId of
                Nothing ->
                    button [ class "sign-in-button", onClick ToggleModalSignIn ] [ text "Sign-in" ]

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
                    [ div [ class "close-modal" ]
                        [ i [ class "fas fa-times", onClick ToggleModalSignIn ] [] ]
                    , User.view model.user |> Html.map User
                    ]
                ]


isActive : ViewType -> ViewType -> ( String, Bool )
isActive is should =
    case is of
        Table ->
            case should of
                Table ->
                    ( "active", True )

                _ ->
                    ( "disactive", True )

        Squart ->
            case should of
                Squart ->
                    ( "active", True )

                _ ->
                    ( "disactive", True )

        _ ->
            ( "disactive", True )
