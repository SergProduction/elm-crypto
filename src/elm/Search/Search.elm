module Search.Search exposing
    ( Model
    , Msg(..)
    , getPairSymbols
    , init
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
import Search.Data.Take as Data


type Msg
    = FindPair String
    | SubscribePair Data.Pair
    | ResponsePairs (Result Http.Error Data.RowPairs)


type alias Model =
    { find : String
    , data : Data.Pairs
    , resultFind : Data.Pairs
    }


init : Model
init =
    { find = ""
    , data = []
    , resultFind = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model=
    case msg of
        FindPair str ->
            ( { model
                | find = str
                , resultFind = List.filter (\d -> String.contains (String.toLower str) (String.toLower d.symbol)) model.data
              }
            , Cmd.none
            )

        SubscribePair _ ->
            ( model, Cmd.none )

        ResponsePairs r ->
            case r of
                Ok d ->
                    let
                        pairs = Data.transformRowPairToPair d
                    in
                    
                    ( { model
                        | data = pairs
                        , resultFind = pairs
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


getPairSymbols : Cmd Msg
getPairSymbols =
    Http.get
        { url = "https://app.coindaq.net/rest/pairs" -- https://coindaq.net:8080 http://142.93.47.26:1023/pairs
        , expect = Http.expectJson ResponsePairs Data.decodeRowPairs
        }


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    input [ class "input-search", type_ "text", placeholder "search", value model.find, onInput FindPair ] []


view : Model -> Dict.Dict String pair -> Html Msg
view model dictPair =
    div [ class "flex-row flex-between" ]
        [ table [ class "full-width" ]
            [ thead [] [ viewHeadRow ]
            , tbody [] (List.map (viewBodyRow dictPair) model.resultFind)
            ]
        ]


viewHeadRow : Html Msg
viewHeadRow =
    tr []
        [ td [ class "name-value-group" ] [ text "Exchange" ]
        , td [ class "name-value-group name" ] [ text "Pair" ]
        , td [ class "name-value-group name" ] [ text "Ask" ]
        , td [ class "name-value-group name" ] [ text "Bid" ]

        , td [ class "name-value-group name" ]
            [ i [ class "fas fa-star" ] [] ]
        ]


viewBodyRow : Dict.Dict String pair -> Data.Pair -> Html Msg
viewBodyRow dictPair row =
    tr []
        [ td [ class "name-value-group" ] [ text row.exchange ]
        , td [ class "name-value-group name" ] [ text row.symbol ]
        , td [ class "name-value-group" ] [ text row.ask ]
        , td [ class "name-value-group" ] [ text row.bid ]
        , td [ class "name-value-group " ]
            [ button [ class "btn transparent blue", onClick (SubscribePair row) ]
                [ if Dict.member ((String.toUpper row.exchange) ++ row.symbol) dictPair then
                    i [ class "fas fa-check green" ] []
                  else
                    text "ADD"
                ]
            ]
        ]
