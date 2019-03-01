port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Data.Pair exposing (Pair, decodePair)
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


type alias PairUnSubResponse =
    { message : Bool
    , pairId : String
    }


type alias PairSub =
    { exchange : String
    , pair : String
    , userId : String
    , pairId : String
    }


type alias PairUnSub =
    { exchange : String
    , pair : String
    , userId : String
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
        , ( "userId", E.string d.userId )
        ]


dencodePairUnSubResponse : D.Decoder PairUnSubResponse
dencodePairUnSubResponse =
    D.map2 PairUnSubResponse
        (D.field "message" D.bool)
        (D.field "pairId" D.string)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = EchoWs (Result D.Error Pair)
    | EchoWsUsub (Result D.Error PairUnSubResponse)
    | WsStatus String
    | User User.Msg
    | Search Search.Msg
    | View ViewType
    | ToggleModalSignIn
    | UnSubPair Pair
    | OpenCtxMenu
    | CloseCtxMenu
    | Leave


type ViewType
    = Table
    | Squart
    | SearchView


type alias Model =
    { data : Dict.Dict String Pair
    , modal : Bool
    , viewType : ViewType
    , contextMenuIsOpen : Bool
    , prevViewType : ViewType
    , isFind : Bool
    , user : User.Model
    , search : Search.Model
    , wsStatus : String
    }


init : String -> ( Model, Cmd Msg )
init session =
    ( { data = Dict.empty
      , viewType = Table
      , contextMenuIsOpen = False
      , prevViewType = Table
      , isFind = False
      , modal = False
      , user = User.init
      , search = Search.init
      , wsStatus = "loading"
      }
    , Cmd.map User (User.getUserInfo session)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        User (User.ResponseSignInSuccess ukey) ->
            let
                ( userModel, command ) =
                    User.update (User.ResponseSignInSuccess ukey) model.user
            in
            ( { model | modal = False, user = userModel }, Cmd.batch [ Cmd.map Search Search.getPairSymbols, Cmd.map User command ] )

        User (User.ResponseUserInfoSuccess userInfo) ->
            let
                ( userModel, command ) =
                    User.update (User.ResponseUserInfoSuccess userInfo) model.user
            in
            ( { model | modal = False, user = userModel }, Cmd.batch [ Cmd.map Search Search.getPairSymbols, Cmd.map User command ] )

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
            case model.user.userId of
                Nothing ->
                    ( model, Cmd.none )

                Just userId ->
                    let
                        pairUnSub =
                            PairUnSub (String.toUpper pair.exchange) pair.symbol userId
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

        OpenCtxMenu ->
            ( { model | contextMenuIsOpen = True }, Cmd.none )

        CloseCtxMenu ->
            ( { model | contextMenuIsOpen = False }, Cmd.none )

        Leave ->
            ( { model | contextMenuIsOpen = False, user = User.init }, leaveUser () )

        WsStatus status ->
            ( { model | wsStatus = status }, Cmd.none )



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

        EchoWsUsub result ->
            case result of
                Ok d ->
                    let
                        data =
                            Dict.filter (\k v -> not (k == d.pairId)) model.data
                    in
                    ( { model | data = data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


port wsListenPairs : (String -> msg) -> Sub msg


port wsListenUnsubcribePairs : (String -> msg) -> Sub msg

port wsDefaultStatus : (String -> msg) -> Sub msg

port toJs : E.Value -> Cmd msg


port leaveUser : () -> Cmd msg



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ wsListenPairs (\s -> EchoWs (D.decodeString decodePair s))
        , wsListenUnsubcribePairs (\s -> EchoWsUsub (D.decodeString dencodePairUnSubResponse s))
        , wsDefaultStatus WsStatus
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewHead model
        , viewModal model
        , div [class "body"]
          [ case model.viewType of
              Table ->
                  viewTable UnSubPair model.data

              Squart ->
                  viewTileList UnSubPair model.data

              SearchView ->
                  Search.view model.search model.data |> Html.map Search
          ]
        , viewStatusBar model
        , if model.contextMenuIsOpen then
            viewContextMenu
          else
            text ""
        ]


viewHead : Model -> Html Msg
viewHead model =
    div [ class "header flex-row flex-between flex-vertical-center roboto" ]
        [ div [ class "header-about-name f-bold" ] [ text "Analytical Monitor" ]
        , div [ class "flex-row" ]
            [ div [ class "red-button flex-row flex-center flex-vertical-center" ]
                [ text "SRH "
                , div [ class "icon-arrow-rigth" ] []
                ]
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
            div [ class "close-search"]
              [ i [ class "fas fa-times", onClick (View SearchView) ] [] ]

          else
            case model.user.userId of
                Nothing ->
                    button [ class "sign-in-button", onClick ToggleModalSignIn ] [ text "Sign-in" ]

                Just _ ->
                    button [ class "sign-in-button", onClick OpenCtxMenu ] [ text model.user.email ]
        ]


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        False ->
            text ""

        True ->
            div [ class "modal-fon flex-row flex-center" ]
              [ div [ class "modal roboto" ]
                  [ i [ class "close-modal fas fa-times", onClick ToggleModalSignIn ] []
                  , div [ class "flex-column modal-form" ]
                      [ User.view model.user |> Html.map User ]
                  ]
              ]


viewStatusBar : Model -> Html Msg
viewStatusBar model =
    div [ classList [("status-bar", True), statusBarColor model.wsStatus ] ]
      [ text <| "Status: "++ model.wsStatus ]


statusBarColor : String -> (String, Bool)
statusBarColor status =
    case status of
        "loading" -> ("blue", True)
        "connect" -> ("green", True)
        "error" -> ("red", True)
        _ -> ("red", True)

       
viewContextMenu : Html Msg
viewContextMenu = div [ class "context-menu", onMouseLeave CloseCtxMenu ]
  [ div [] [ a [ href "https://cp.coindaq.net", class "mango" ] [ text "Control Pannel" ] ]
  , div [] [ a [ href "https://cp.coindaq.net/profile", class "mango" ] [ text "Setting" ] ]
  , div [] [ button [class "btn transparent f-bold mango", onClick Leave ] [ text "Logout" ] ]
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
