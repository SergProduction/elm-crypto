module User exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E


decodeSucces : D.Decoder String
decodeSucces =
    D.field "userkey" D.string


decodeError : D.Decoder String
decodeError =
    D.field "error" D.string


encode : Model -> E.Value
encode model =
    E.object
        [ ( "e", E.string model.email )
        , ( "p", E.string model.password )
        ]


type alias Model =
    { email : String
    , password : String
    , userId : Maybe String
    , responseError : Maybe String
    }


type Msg
    = Email String
    | Password String
    | Submit
    | ResponseSuccess String
    | ResponseError String
    | ResponseFail
    | ResponseIsNotValide


init : Model
init =
    { email = ""
    , password = ""
    , userId = Nothing
    , responseError = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email str ->
            ( { model | email = str }, Cmd.none )

        Password str ->
            ( { model | password = str }, Cmd.none )

        Submit ->
            ( model, singUp model )

        ResponseSuccess ukey ->
            ( { model | userId = Just ukey }, Cmd.none )

        ResponseError message ->
            ( { model | responseError = Just message }, Cmd.none )

        ResponseIsNotValide ->
            ( model, Cmd.none )

        ResponseFail ->
            ( model, Cmd.none )


decodeSuccessOrError : Result.Result Http.Error String -> Msg
decodeSuccessOrError r =
    case r of
        Ok response ->
            case D.decodeString decodeSucces response of
                Ok success ->
                    ResponseSuccess success

                Err _ ->
                    case D.decodeString decodeError response of
                        Ok errorMessage ->
                            ResponseError errorMessage

                        Err e ->
                            ResponseIsNotValide

        Err e ->
            ResponseFail


singUp : Model -> Cmd Msg
singUp model =
    Http.post
        { url = "https://cp.coindaq.net/api/getuserkey"
        , body = Http.jsonBody (encode model)
        , expect = Http.expectString decodeSuccessOrError
        }


view : Model -> Html Msg
view model =
    div []
        [ case model.responseError of
            Nothing ->
                text ""

            Just errorMessage ->
                div [ class "input-group" ]
                    [ div [ class "error" ] [ text errorMessage ] ]
        , div [ class "input-group" ]
            [ input [ type_ "text", placeholder "Email", onInput Email, value model.email ] []
            ]
        , div [ class "input-group" ]
            [ input [ type_ "text", placeholder "Password", onInput Password, value model.password ] []
            ]
        , div [ class "input-group" ]
            [ button [ class "button-form", onClick Submit ] [ text "Connect" ]
            ]
        ]
