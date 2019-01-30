port module Main exposing (..)

import Browser exposing (element)
import Html exposing (button, div, Html, input, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Credentials =
    { login : String
    , password : String
    }


type alias CredentialsInputModel =
    { login : Maybe String
    , password : Maybe String
    , reasonAuthenticationFailed : Maybe ReasonAuthenticationFailed
    }


type alias Meeting =
    { name : String }


type alias MeetingsModel =
    List Meeting


type LoginModel
    = CredentialsInput CredentialsInputModel
    | Verification


type Model
    = Login LoginModel
    | Menu
    | Meetings MeetingsModel
    | CreateAMeeting


type ReasonAuthenticationFailed
    = LoginWasNotTypedIn
    | LoginAndPasswordWereIncorrect
    | NeitherLoginNorPasswordWereTypedIn
    | NetworkError
    | PasswordWasNotTypedIn


emptyCredentialsInputModel : CredentialsInputModel
emptyCredentialsInputModel =
    { login = Nothing
    , password = Nothing
    , reasonAuthenticationFailed = Nothing
    }


emptyLoginModel : LoginModel
emptyLoginModel =
    CredentialsInput emptyCredentialsInputModel


emptyModel : Model
emptyModel =
    Login emptyLoginModel


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | LoginButtonClicked
    | LoginTypedIn String
    | PasswordTypedIn String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoginButtonClicked ->
            ( updateLoginButtonClicked model, updateLoginButtonClickedCommand model )

        LoginTypedIn login ->
            ( updateLoginTypedIn model login, Cmd.none )

        PasswordTypedIn password ->
            ( updatePasswordTypedIn model password, Cmd.none )


updateLoginButtonClicked : Model -> Model
updateLoginButtonClicked model =
    case model of
        Login loginModel ->
            updateLoginButtonClickedLogin loginModel |> Login

        _ ->
            model


updateLoginButtonClickedCommand : Model -> Cmd Msg
updateLoginButtonClickedCommand model =
    case model of
        Login loginModel ->
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    case ( credentialsInputModel.login, credentialsInputModel.password ) of
                        ( Just login, Just password ) ->
                            verifyCredentials { login = login, password = password }

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateLoginButtonClickedLogin : LoginModel -> LoginModel
updateLoginButtonClickedLogin loginModel =
    case loginModel of
        CredentialsInput credentialsInputModel ->
            let
                newCredentialsInputModel =
                    updateLoginButtonClickedLoginCredentialsInput credentialsInputModel
            in
                if newCredentialsInputModel.reasonAuthenticationFailed == Nothing then
                    Verification
                else
                    CredentialsInput newCredentialsInputModel

        Verification ->
            loginModel


updateLoginButtonClickedLoginCredentialsInput : CredentialsInputModel -> CredentialsInputModel
updateLoginButtonClickedLoginCredentialsInput credentialsInputModel =
    let
        newCredentialsInputModel =
            case ( credentialsInputModel.login, credentialsInputModel.password ) of
                ( Nothing, Nothing ) ->
                    { credentialsInputModel | reasonAuthenticationFailed = Just NeitherLoginNorPasswordWereTypedIn }

                ( Just login, Nothing ) ->
                    { credentialsInputModel | reasonAuthenticationFailed = Just LoginWasNotTypedIn }

                ( Nothing, Just password ) ->
                    { credentialsInputModel | reasonAuthenticationFailed = Just PasswordWasNotTypedIn }

                ( Just login, Just password ) ->
                    { credentialsInputModel | reasonAuthenticationFailed = Nothing }
    in
        newCredentialsInputModel


updateLoginTypedIn : Model -> String -> Model
updateLoginTypedIn model login =
    case model of
        Login loginModel ->
            updateLoginTypedInLogin loginModel login
                |> Login

        _ ->
            model


updateLoginTypedInLogin : LoginModel -> String -> LoginModel
updateLoginTypedInLogin loginModel login =
    case loginModel of
        CredentialsInput credentialsInputModel ->
            updateLoginTypedInLoginCredentialsInput credentialsInputModel login
                |> CredentialsInput

        _ ->
            loginModel


updateLoginTypedInLoginCredentialsInput : CredentialsInputModel -> String -> CredentialsInputModel
updateLoginTypedInLoginCredentialsInput credentialsInputModel login =
    if String.length login == 0 then
        { credentialsInputModel | login = Nothing }
    else
        { credentialsInputModel | login = Just login }


updatePasswordTypedIn : Model -> String -> Model
updatePasswordTypedIn model password =
    case model of
        Login loginModel ->
            updatePasswordTypedInLogin loginModel password
                |> Login

        _ ->
            model


updatePasswordTypedInLogin : LoginModel -> String -> LoginModel
updatePasswordTypedInLogin loginModel password =
    case loginModel of
        CredentialsInput credentialsInputModel ->
            updatePasswordTypedInLoginCredentialsInput credentialsInputModel password
                |> CredentialsInput

        _ ->
            loginModel


updatePasswordTypedInLoginCredentialsInput : CredentialsInputModel -> String -> CredentialsInputModel
updatePasswordTypedInLoginCredentialsInput credentialsInputModel password =
    if String.length password == 0 then
        { credentialsInputModel | password = Nothing }
    else
        { credentialsInputModel | password = Just password }


port verifyCredentials : Credentials -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


getErrorText : Maybe ReasonAuthenticationFailed -> String
getErrorText reasonAuthenticationFailed =
    case reasonAuthenticationFailed of
        Just reason ->
            case reason of
                LoginWasNotTypedIn ->
                    "Please type in your login and try again"

                LoginAndPasswordWereIncorrect ->
                    "Sorry, your credentials could not be verified"

                NeitherLoginNorPasswordWereTypedIn ->
                    "Please type in both, your login and your password, and try again"

                NetworkError ->
                    "There was a problem connecting to the server"

                PasswordWasNotTypedIn ->
                    "Please type in your password and try again"

        Nothing ->
            ""


view : Model -> Html Msg
view model =
    case model of
        Login loginModel ->
            viewLogin loginModel

        Menu ->
            viewMenu

        Meetings meetingsModel ->
            viewMeetings

        CreateAMeeting ->
            viewCreateAMeeting


viewCreateAMeeting : Html Msg
viewCreateAMeeting =
    div [] [ text "Create a meeting" ]


viewMeetings : Html Msg
viewMeetings =
    div [] [ text "Meetings" ]


viewMenu : Html Msg
viewMenu =
    div [] [ text "Menu" ]


viewLogin : LoginModel -> Html Msg
viewLogin loginModel =
    case loginModel of
        CredentialsInput credentialsInputModel ->
            div
                []
                [ text "Login"
                , input [ onInput LoginTypedIn, type_ "text" ] []
                , input [ onInput PasswordTypedIn, type_ "password" ] []
                , div [] [ getErrorText credentialsInputModel.reasonAuthenticationFailed |> text ]
                , button [ onClick LoginButtonClicked ] [ text "Login" ]
                ]

        Verification ->
            div
                []
                [ text "Verifying your credentials..." ]
