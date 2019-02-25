port module User exposing (Model, Msg(..), init, update, view, getUserInfo)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Regex


decodeSignInSucces : D.Decoder String
decodeSignInSucces =
    D.field "userkey" D.string


decodeSignInError : D.Decoder String
decodeSignInError =
    D.field "error" D.string


decodeUserInfoSucces : D.Decoder UserInfo
decodeUserInfoSucces =
    D.map5 UserInfo
      (D.field "id" D.string)
      (D.field "userkey" D.string)
      (D.field "email" D.string)
      (D.field "firstname" (D.maybe D.string))
      (D.field "lastname" (D.maybe D.string))



decodeUserInfoError : D.Decoder String
decodeUserInfoError =
    D.field "error" D.string


encodeResponse : Model -> E.Value
encodeResponse model =
    E.object
        [ ( "e", E.string model.email )
        , ( "p", E.string model.password )
        ]

type alias UserInfo =
  { id: String
  , userId: String
  , email: String
  , firstname: Maybe String
  , lastname: Maybe String
  }

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
    | ResponseSignInSuccess String
    | ResponseSignInError String
    | ResponseSignInFail
    | ResponseSignInNotValid
    | ResponseUserInfoSuccess UserInfo
    | ResponseUserInfoError String
    | ResponseUserInfoFail
    | ResponseUserInfoNotValid


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

            

        ResponseSignInSuccess ukey ->
            ( { model | userId = Just ukey }, saveSession <| encodeSession <| Session ukey model.email )

        ResponseSignInError message ->
            ( { model | responseError = Just message }, Cmd.none )

        ResponseSignInNotValid ->
            ( model, Cmd.none )

        ResponseSignInFail ->
            ( model, Cmd.none )


        ResponseUserInfoSuccess userInfo ->
            ( { model | userId = Just userInfo.userId }, Cmd.none )

        ResponseUserInfoError message ->
            ( model, Cmd.none )

        ResponseUserInfoNotValid ->
            ( model, Cmd.none )

        ResponseUserInfoFail ->
            ( model, Cmd.none )


port saveSession : E.Value -> Cmd msg


decodeSignInSuccessOrError : Result.Result Http.Error String -> Msg
decodeSignInSuccessOrError r =
    case r of
        Ok response ->
            case D.decodeString decodeSignInSucces response of
                Ok success ->
                    ResponseSignInSuccess success

                Err _ ->
                    case D.decodeString decodeSignInError response of
                        Ok errorMessage ->
                            ResponseSignInError errorMessage

                        Err e ->
                            ResponseSignInNotValid

        Err e ->
            ResponseSignInFail


decodeUserInfoSuccessOrError : Result.Result Http.Error String -> Msg
decodeUserInfoSuccessOrError r =
    case r of
        Ok response ->
            case D.decodeString decodeUserInfoSucces response of
                Ok success ->
                    ResponseUserInfoSuccess success

                Err _ ->
                    case D.decodeString decodeUserInfoError response of
                        Ok errorMessage ->
                            ResponseUserInfoError errorMessage

                        Err e ->
                            ResponseUserInfoNotValid

        Err e ->
            ResponseUserInfoFail

singUp : Model -> Cmd Msg
singUp model =
    Http.post
        { url = "https://cp.coindaq.net/api/getuserkey"
        , body = Http.jsonBody (encodeResponse model)
        , expect = Http.expectString decodeSignInSuccessOrError
        }


getUserInfo : Cmd Msg
getUserInfo =
  Http.post
        { url = "https://cp.coindaq.net/api/getuserinfo"
        , body = Http.emptyBody
        , expect = Http.expectString decodeUserInfoSuccessOrError
        }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "input-group flex-center" ] [ text "CoinDAQ Terminal" ]
        , case (model.responseError, model.emailValid) of
            (Nothing, Nothing) ->
                text ""
            
            (_, Just errorMessage) ->
                div [ class "input-group" ]
                    [ div [ class "error" ] [ text errorMessage ] ]

            (Just errorMessage, _) ->
                div [ class "input-group" ]
                    [ div [ class "error" ] [ text errorMessage ] ]
        , div [ class "input-group" ]
            [ input [ class "green", type_ "text", placeholder "Email", onInput Email, value model.email ] []
            ]
        , div [ class "input-group" ]
            [ input [ class "red", type_ "password", placeholder "Password", onInput Password, value model.password ] []
            ]
        , div [ class "input-group" ]
            [ button [ class "button-form", onClick Submit ] [ text "Sign In" ]
            ]
        , div [ class "input-group flex-between" ]
            [ a [ href "#fp", class "mango" ] [ text "Forgot password?" ]
            , a [ href "#su", class "blue" ] [ text "Sign up ->" ]
            ]
        ]
