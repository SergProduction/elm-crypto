module Search exposing
    ( JSONPairSymbols
    , Model
    , Msg(..)
    , PairSymbolsList
    , decodeJSONPairSymbols
    , getPairSymbols
    , init
    , transformJSONToPairSymbolsList
    , update
    , view
    , viewSearchInput
    )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E


type alias JSONPairSymbols =
    Dict.Dict String (List String)


type alias PairSymbols =
    { exchange : String
    , pair : String
    }


type alias PairSymbolsList =
    List PairSymbols


transformJSONToPairSymbolsList : JSONPairSymbols -> PairSymbolsList
transformJSONToPairSymbolsList a =
    List.concatMap
        (\( exchange, pairs ) -> List.map (\pair -> PairSymbols exchange pair) pairs)
        (Dict.toList a)


decodeJSONPairSymbols : D.Decoder JSONPairSymbols
decodeJSONPairSymbols =
    D.dict (D.list D.string)


type Msg
    = FindPair String
    | SubscribePair PairSymbols
    | ResponsePairSymbols (Result Http.Error JSONPairSymbols)


type alias Model =
    { find : String
    , data : PairSymbolsList
    , resultFind : PairSymbolsList
    }


init : Model
init =
    { find = ""
    , data = []
    , resultFind = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindPair str ->
            ( { model
                | find = str
                , resultFind = List.filter (\d -> String.contains (String.toLower str) (String.toLower d.pair)) model.data
              }
            , Cmd.none
            )

        SubscribePair _ ->
            ( model, Cmd.none )

        ResponsePairSymbols r ->
            case r of
                Ok d ->
                    ( { model
                        | data = transformJSONToPairSymbolsList d
                        , resultFind = transformJSONToPairSymbolsList d
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


getPairSymbols : Cmd Msg
getPairSymbols =
    Http.get
        { url = "https://app.coindaq.net/rest/pairs" -- https://coindaq.net:8080 http://142.93.47.26:1023/pairs
        , expect = Http.expectJson ResponsePairSymbols decodeJSONPairSymbols
        }


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    input [ class "input-search", type_ "text", placeholder "search", value model.find, onInput FindPair ] []


view : Model -> Html Msg
view model =
    div [ class "search flex-row flex-between" ]
        [ table [ class "full-width" ]
            [ thead [] [ viewHeadRow ]
            , tbody [] (List.map viewBodyRow model.resultFind)
            ]
        ]


viewHeadRow : Html Msg
viewHeadRow =
    tr []
        [ td [ class "name-value-group" ] [ text "Exchange" ]
        , td [ class "name-value-group name" ] [ text "Pair" ]
        , td [ class "name-value-group name" ]
            [ i [ class "fas fa-star" ] [] ]
        ]


viewBodyRow : PairSymbols -> Html Msg
viewBodyRow row =
    tr []
        [ td [ class "name-value-group" ] [ text row.exchange ]
        , td [ class "name-value-group name" ] [ text row.pair ]
        , td [ class "name-value-group " ]
            [ button [ class "btn transparent blue", onClick (SubscribePair row) ] [ text "ADD" ] ]
        ]
