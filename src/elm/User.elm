port module User exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Regex


decodeSucces : D.Decoder String
decodeSucces =
    D.field "userkey" D.string


decodeError : D.Decoder String
decodeError =
    D.field "error" D.string


encodeResponse : Model -> E.Value
encodeResponse model =
    E.object
        [ ( "e", E.string model.email )
        , ( "p", E.string model.password )
        ]

type alias Session =
  { userId : String
  , email : String
  }

encodeSession : Session -> E.Value
encodeSession s =
    E.object
        [ ( "userId", E.string s.userId )
        , ( "email", E.string s.email )
        ]

decodeSession : D.Decoder Session
decodeSession =
    D.map2 Session
        ( D.field "userId" D.string )
        ( D.field "email" D.string )


type alias Model =
    { email : String
    , password : String
    , userId : Maybe String
    , responseError : Maybe String
    , emailValid : Maybe String
    }


type Msg
    = Email String
    | Password String
    | Submit
    | ResponseSuccess String
    | ResponseError String
    | ResponseFail
    | ResponseIsNotValide


init : String -> Model
init initSession =
  let
    sess = case D.decodeString decodeSession initSession of
      Ok s -> s
      Err _ -> Session "" ""
  in
    { email = sess.email
    , password = ""
    , userId = if sess.userId == "" then Nothing else Just sess.userId
    , responseError = Nothing
    , emailValid = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email str ->
            ( { model | email = str }, Cmd.none )

        Password str ->
            ( { model | password = str }, Cmd.none )

        Submit ->
          let
              regex = Regex.fromString "\\S+@\\S+\\.\\S+"
          in
            case regex of
                Nothing -> ( model, Cmd.none )
                 
            
                Just r ->
                  if Regex.contains r model.email then
                    ( model, singUp model )
                  else
                    ( { model | emailValid = Just "email is not valid" }, Cmd.none )

            

        ResponseSuccess ukey ->
            ( { model | userId = Just ukey }, saveSession <| encodeSession <| Session ukey model.email )

        ResponseError message ->
            ( { model | responseError = Just message }, Cmd.none )

        ResponseIsNotValide ->
            ( model, Cmd.none )

        ResponseFail ->
            ( model, Cmd.none )


port saveSession : E.Value -> Cmd msg


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
        , body = Http.jsonBody (encodeResponse model)
        , expect = Http.expectString decodeSuccessOrError
        }


view : Model -> Html Msg
view model =
    div []
        [ case (model.responseError, model.emailValid) of
            (Nothing, Nothing) ->
                text ""
            
            (_, Just errorMessage) ->
                div [ class "input-group" ]
                    [ div [ class "error" ] [ text errorMessage ] ]

            (Just errorMessage, _) ->
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
