port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Data as DataModule exposing (Data, decode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MAIN


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Begin ws" ]
        , table [] (List.map viewCeil model)
        ]


viewCeil : Data -> Html Msg
viewCeil d =
    tr []
        [ td [] [ text d.symbol ]
        , td [] [ text d.exchange ]
        ]
