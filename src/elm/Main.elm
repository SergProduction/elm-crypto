port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict
import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Search.Search as Search
import User
import View.Table exposing (viewTable)
import View.Tile exposing (viewTileList)
import Data.TakePair as TakePair
import Data.TakeUnSub as TakeUnSub
import Data.SendSub as SendSub
import Data.SendUnSub as SendUnSub
import MainHelpers exposing (shiftData)



main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = EchoWs (Result D.Error TakePair.Pair)
    | EchoWsUnSub (Result D.Error TakeUnSub.PairUnSub)
    | WsStatus String
    | User User.Msg
    | Search Search.Msg
    | View ViewType
    | ToggleModalSignIn
    | PairMoveTop
    | PairMoveUp
    | PairMoveDow
    | UnSubPair
    | OpenCtxMenuUser
    | OpenCtxMenuPair TakePair.Pair
    | CloseCtxMenu
    | LogOut


type ViewType
    = Table
    | Squart
    | SearchView


type alias Model =
    { dataMap : Dict.Dict String Int
    , data : Array.Array TakePair.Pair
    , modal : Bool
    , viewType : ViewType
    , contextMenuUserIsOpen : Bool
    , contextMenuPairIsOpen : Bool
    , contextMenuPair : Maybe TakePair.Pair
    , prevViewType : ViewType
    , isFind : Bool
    , user : User.Model
    , search : Search.Model
    , wsStatus : String
    }


createPairId : String -> String -> String
createPairId exchange symbol = String.toUpper <| exchange ++ symbol


init : String -> ( Model, Cmd Msg )
init session =
    ( { dataMap = Dict.empty
      , data = Array.empty
      , viewType = Table
      , contextMenuUserIsOpen = False
      , contextMenuPairIsOpen = False
      , contextMenuPair = Nothing
      , prevViewType = Table
      , isFind = False
      , modal = False
      , user = User.init
      , search = Search.init
      , wsStatus = "Loading"
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
                        SendSub.encode <|
                            SendSub.PairSub (String.toUpper p.exchange) p.symbol userId ( createPairId p.exchange p.symbol)
                    )


        Search searchMsg ->
            let
                ( search, command ) =
                    Search.update searchMsg model.search
            in
            ( { model | search = search }, Cmd.map Search command )

        UnSubPair ->
            let
              maybeModelCmd =
                Maybe.map2
                  (\userId ctxPair ->
                    let
                      pairUnSub =
                        SendUnSub.PairUnSub
                          (String.toUpper ctxPair.exchange)
                          ctxPair.id
                          userId
                    in
                    ( model, toJs (SendUnSub.encode pairUnSub) )
                  )
                  model.user.userId
                  model.contextMenuPair
            in
              case maybeModelCmd of
                Just ( newModel, cmd ) ->
                    ( newModel, cmd )
                Nothing ->
                    ( model, Cmd.none )

        View vtype ->
            case vtype of
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

        OpenCtxMenuUser ->
            ( { model | contextMenuUserIsOpen = True }, Cmd.none )

        OpenCtxMenuPair pair->
            ( { model | contextMenuPairIsOpen = True, contextMenuPair = Just pair }, Cmd.none )

        CloseCtxMenu ->
            (
              { model
              | contextMenuUserIsOpen = False
              , contextMenuPairIsOpen = False
              , contextMenuPair = Nothing
              }
            ,
              Cmd.none
            )

        LogOut ->
            ( { model | contextMenuUserIsOpen = False, user = User.init }, leaveUser () )

        WsStatus status ->
            ( { model | wsStatus = status }, Cmd.none )

        PairMoveTop ->
          case model.contextMenuPair of
            Nothing ->
              ( model , Cmd.none )

            Just pair ->
                let
                  maybeData = shiftData (model.dataMap, model.data) pair MainHelpers.Top
                in
                  case maybeData of
                    Nothing ->
                      ( model , Cmd.none )
        
                    Just (dataMap, data) ->

                      ( { model | dataMap = dataMap, data = data } , Cmd.none )

        PairMoveUp ->
          case model.contextMenuPair of
            Nothing ->
              ( model , Cmd.none )

            Just pair ->
                let
                  maybeData = shiftData (model.dataMap, model.data) pair MainHelpers.Up
                in
                  case maybeData of
                    Nothing ->
                      let
                          x = Debug.log "ff" "f"
                      in
                      
                      ( model , Cmd.none )
        
                    Just (dataMap, data) ->

                      ( { model | dataMap = dataMap, data = data } , Cmd.none )
              
        PairMoveDow ->
          case model.contextMenuPair of
            Nothing ->
              ( model , Cmd.none )

            Just pair ->
                let
                  maybeData = shiftData (model.dataMap, model.data) pair MainHelpers.Down
                in
                  case maybeData of
                    Nothing ->
                      ( model , Cmd.none )
        
                    Just (dataMap, data) ->

                      ( { model | dataMap = dataMap, data = data } , Cmd.none )

        EchoWs result ->
            case result of
                Ok newPair ->
                    let
                        data =
                            if Dict.member newPair.id model.dataMap then
                              case Dict.get newPair.id model.dataMap of
                                Just index -> Array.set index newPair model.data
                                Nothing -> model.data
                            else
                              Array.push newPair model.data
                        dataMap =
                            if Dict.member newPair.id model.dataMap then
                              model.dataMap
                            else
                              Dict.insert
                                newPair.id
                                ((Array.length data) - 1 )
                                model.dataMap
                    in
                    ( { model | data = data, dataMap = dataMap }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        EchoWsUnSub result ->
            case result of
                Ok unsubPair ->
                    let
                        data =
                          Array.filter (\p -> not (p.id == unsubPair.pairId)) model.data
                        dataMap =
                            Dict.filter
                              (\k v -> not (k == unsubPair.pairId))
                              model.dataMap
                    in
                    ( { model | data = data, dataMap = dataMap }, Cmd.none )

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
        [ wsListenPairs (\s -> EchoWs (D.decodeString TakePair.decode s))
        , wsListenUnsubcribePairs (\s -> EchoWsUnSub (D.decodeString TakeUnSub.dencode s))
        , wsDefaultStatus WsStatus
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewHead model
        , viewModal model
        , div [ class "body" ]
          [ case model.viewType of
              Table ->
                  div [ class "base" ] [ (viewTable OpenCtxMenuPair model.data) ]

              Squart ->
                  div [ class "base" ] [ (viewTileList OpenCtxMenuPair model.data) ]

              SearchView ->
                  div [ class "search" ] [ (Search.view model.search model.dataMap |> Html.map Search) ]
          ]
        , viewStatusBar model
        , if model.contextMenuUserIsOpen then
            viewContextMenu
          else
            text ""
        , if model.contextMenuPairIsOpen then
            viewContextMenuPair
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
        , div [ class "market-cap" ] [] -- text "Market Cap: $120 558 456 737 • 24h Vol: $20 850 816 957 • BTC Dominance: 52.7%" ]
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
                    button [ class "sign-in-button", onClick OpenCtxMenuUser ] [ text model.user.email ]
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
    div [ class "status-bar flex-row flex-between" ]
      [ div [ classList [statusBarColor model.wsStatus] ] [ text <| "Status: "++ model.wsStatus ]
      , div [] [ text "Terminal MVP: 0.9.4"]
      ]


statusBarColor : String -> (String, Bool)
statusBarColor status =
    case status of
        "Loading" -> ("blue", True)
        "Connect" -> ("green", True)
        "Error" -> ("red", True)
        _ -> ("red", True)

       
viewContextMenu : Html Msg
viewContextMenu = div [ class "context-menu", onMouseLeave CloseCtxMenu ]
  [ div [] [ a [ href "https://cp.coindaq.net", class "mango" ] [ text "Control Pannel" ] ]
  , div [] [ a [ href "https://cp.coindaq.net/profile", class "mango" ] [ text "Setting" ] ]
  , div [] [ button [class "btn transparent f-bold mango", onClick LogOut ] [ text "Logout" ] ]
  ]

viewContextMenuPair : Html Msg
viewContextMenuPair = div [ class "context-menu", onMouseLeave CloseCtxMenu ]
  [ div [] [ button [class "btn transparent f-bold mango", onClick PairMoveTop ] [ text "Move top" ] ]
  , div [] [ button [class "btn transparent f-bold mango", onClick PairMoveUp ] [ text "Move up" ] ]
  , div [] [ button [class "btn transparent f-bold mango", onClick PairMoveDow ] [ text "Move down" ] ]
  , div [] [ button [class "btn transparent f-bold mango", onClick UnSubPair ] [ text "Remove" ] ]
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
