module User exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E


decode : D.Decoder String
decode =
    D.field "userkey" D.string


encode : Model -> E.Value
encode model =
    E.object
        [ ( "e", E.string model.email )
        , ( "p", E.string model.password )
        ]


type alias Model =
    { email : String
    , password : String
    , userkey : String
    }


type Msg
    = Email String
    | Password String
    | Submit
    | Response (Result Http.Error String)


init : Model
init =
    { email = ""
    , password = ""
    , userkey = ""
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

        Response result ->
            case result of
                Ok ukey ->
                    ( { model | userkey = ukey }, Cmd.none )

                Err e ->
                    let
                        f =
                            Debug.log "Response" e
                    in
                    ( model, Cmd.none )


singUp : Model -> Cmd Msg
singUp model =
    Http.post
        { url = "https://cp.coindaq.net/api/getuserkey"
        , body = Http.jsonBody (encode model)
        , expect = Http.expectJson Response decode
        }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "input-group" ]
            [ input [ type_ "text", placeholder "Email", onInput Email, value model.email ] []
            ]
        , div [ class "input-group" ]
            [ input [ type_ "text", placeholder "Password", onInput Password, value model.password ] []
            ]
        , div [ class "input-group" ]
            [ button [ class "button-form", onClick Submit ] [ text "Connect" ]
            ]
        ]
