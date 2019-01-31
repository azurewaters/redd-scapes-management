port module Main exposing (..)

import Browser exposing (element)
import Html exposing (button, div, Html, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, field, string, map)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias AuthenticatedUser =
    { email : String
    , uid : String
    }


type alias Credentials =
    { login : String
    , password : String
    }


type alias CredentialsInputModel =
    { login : String
    , password : String
    , reasonAuthenticationFailed : Maybe ReasonAuthenticationFailed
    }


type alias Meeting =
    { name : String }


type alias MeetingsModel =
    { authenticatedUser : AuthenticatedUser
    , meetings : List Meeting
    }


type LoginModel
    = CredentialsInput CredentialsInputModel
    | Verification


type Model
    = Login LoginModel
    | Menu AuthenticatedUser
    | Meetings MeetingsModel
    | CreateOrEditAMeeting (Maybe Meeting)


type ReasonAuthenticationFailed
    = EmailWasInvalid
    | LoginAndPasswordWereIncorrect
    | LoginWasNotFound
    | LoginWasNotTypedIn
    | NeitherLoginNorPasswordWereTypedIn
    | NetworkError
    | PasswordWasNotTypedIn
    | PasswordWasWrong
    | UnknownError
    | UserWasDisabled


emptyAuthenticatedUser : AuthenticatedUser
emptyAuthenticatedUser =
    { email = ""
    , uid = ""
    }


emptyCredentialsInputModel : CredentialsInputModel
emptyCredentialsInputModel =
    { login = ""
    , password = ""
    , reasonAuthenticationFailed = Nothing
    }


emptyLoginModel : LoginModel
emptyLoginModel =
    CredentialsInput emptyCredentialsInputModel


emptyMeetingsModel : MeetingsModel
emptyMeetingsModel =
    { authenticatedUser = emptyAuthenticatedUser
    , meetings = []
    }


emptyModel : Model
emptyModel =
    Login emptyLoginModel


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = AuthenticationFailed String
    | AuthenticatedSucceeded AuthenticatedUser
    | AuthenticatedUserSignedOut
    | LoginLoginButtonClicked
    | LoginLoginTypedIn String
    | LoginPasswordTypedIn String
    | MenuMeetingsButtonClicked
    | MenuSignOutButtonClicked
    | NoOp


getReasonAuthenticationFailedUsingAuthenticationFailedErrorCode : String -> Maybe ReasonAuthenticationFailed
getReasonAuthenticationFailedUsingAuthenticationFailedErrorCode authenticationFailedErrorCode =
    case authenticationFailedErrorCode of
        "auth/invalid-email" ->
            Just EmailWasInvalid

        "auth/user-disabled" ->
            Just UserWasDisabled

        "auth/user-not-found" ->
            Just LoginWasNotFound

        "auth/wrong-password" ->
            Just PasswordWasWrong

        "auth/network-request-failed" ->
            Just NetworkError

        _ ->
            Just UnknownError


getReasonAuthenticationFailedUsingLoginAndPassword : String -> String -> Maybe ReasonAuthenticationFailed
getReasonAuthenticationFailedUsingLoginAndPassword login password =
    case ( String.isEmpty login, String.isEmpty password ) of
        ( True, True ) ->
            Just NeitherLoginNorPasswordWereTypedIn

        ( True, False ) ->
            Just LoginWasNotTypedIn

        ( False, True ) ->
            Just PasswordWasNotTypedIn

        ( False, False ) ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthenticationFailed authenticationFailedErrorCode ->
            case model of
                Login loginModel ->
                    case loginModel of
                        Verification ->
                            ( { login = "", password = "", reasonAuthenticationFailed = getReasonAuthenticationFailedUsingAuthenticationFailedErrorCode authenticationFailedErrorCode }
                                |> CredentialsInput
                                |> Login
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AuthenticatedUserSignedOut ->
            ( Login emptyLoginModel, Cmd.none )

        AuthenticatedSucceeded authenticatedUser ->
            case model of
                Login loginModel ->
                    ( Menu, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoginLoginButtonClicked ->
            ( updateLoginButtonClicked model
            , updateLoginButtonClickedCommand model
            )

        LoginLoginTypedIn login ->
            ( updateLoginTypedIn model login, Cmd.none )

        LoginPasswordTypedIn password ->
            ( updatePasswordTypedIn model password, Cmd.none )

        MenuMeetingsButtonClicked ->
            ( Meetings emptyMeetingsModel, Cmd.none )

        MenuSignOutButtonClicked ->
            ( model, signOut True )

        NoOp ->
            ( model, Cmd.none )


updateLoginButtonClicked : Model -> Model
updateLoginButtonClicked model =
    case model of
        Login loginModel ->
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    let
                        reasonAuthenticationFailed =
                            getReasonAuthenticationFailedUsingLoginAndPassword credentialsInputModel.login credentialsInputModel.password
                    in
                        if reasonAuthenticationFailed == Nothing then
                            Verification
                                |> Login
                        else
                            { credentialsInputModel | reasonAuthenticationFailed = reasonAuthenticationFailed }
                                |> CredentialsInput
                                |> Login

                _ ->
                    model

        _ ->
            model


updateLoginButtonClickedCommand : Model -> Cmd Msg
updateLoginButtonClickedCommand model =
    case model of
        Login loginModel ->
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    if (credentialsInputModel.login /= "") && (credentialsInputModel.password /= "") then
                        verifyCredentials { login = credentialsInputModel.login, password = credentialsInputModel.password }
                    else
                        Cmd.none

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateLoginTypedIn : Model -> String -> Model
updateLoginTypedIn model login =
    case model of
        Login loginModel ->
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    { credentialsInputModel | login = login }
                        |> CredentialsInput
                        |> Login

                _ ->
                    Login loginModel

        _ ->
            model


updatePasswordTypedIn : Model -> String -> Model
updatePasswordTypedIn model password =
    case model of
        Login loginModel ->
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    { credentialsInputModel | password = password }
                        |> CredentialsInput
                        |> Login

                _ ->
                    Login loginModel

        _ ->
            model


port verifyCredentials : Credentials -> Cmd msg


port signOut : Bool -> Cmd msg



-- SUBSCRIPTIONS


authenticatedUserDecoder : Decoder AuthenticatedUser
authenticatedUserDecoder =
    Decode.map2 AuthenticatedUser emailDecoder uidDecoder


emailDecoder : Decoder String
emailDecoder =
    Decode.field "email" Decode.string


uidDecoder : Decoder String
uidDecoder =
    Decode.field "uid" Decode.string


decodeAuthenticationFailedFromJS : Decode.Value -> Msg
decodeAuthenticationFailedFromJS receivedValue =
    let
        resultOfDecoding =
            Decode.decodeValue Decode.string receivedValue
    in
        case resultOfDecoding of
            Ok errorCode ->
                AuthenticationFailed errorCode

            Err error ->
                NoOp


decodeAuthenticatedUserSignedOutFromJS : Decode.Value -> Msg
decodeAuthenticatedUserSignedOutFromJS receivedValue =
    let
        resultOfDecoding =
            Decode.decodeValue Decode.bool receivedValue
    in
        case resultOfDecoding of
            Ok authenticationFailed ->
                if authenticationFailed then
                    AuthenticatedUserSignedOut
                else
                    NoOp

            Err error ->
                NoOp


decodeAuthenticatedUserFromJS : Decode.Value -> Msg
decodeAuthenticatedUserFromJS receivedValue =
    let
        resultOfDecoding =
            Decode.decodeValue authenticatedUserDecoder receivedValue
    in
        case resultOfDecoding of
            Ok newAuthenticatedUser ->
                AuthenticatedSucceeded newAuthenticatedUser

            Err error ->
                AuthenticatedUserSignedOut


port authenticationFailedFromJS : (Decode.Value -> msg) -> Sub msg


port authenticationSucceededFromJS : (Decode.Value -> msg) -> Sub msg


port authenticatedUserSignedOutFromJS : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authenticationFailedFromJS decodeAuthenticationFailedFromJS
        , authenticationSucceededFromJS decodeAuthenticatedUserFromJS
        , authenticatedUserSignedOutFromJS decodeAuthenticatedUserSignedOutFromJS
        ]



-- VIEW


getErrorTextUsingReasonAuthenticationFailed : Maybe ReasonAuthenticationFailed -> String
getErrorTextUsingReasonAuthenticationFailed reasonAuthenticationFailed =
    case reasonAuthenticationFailed of
        Just reason ->
            case reason of
                LoginWasNotTypedIn ->
                    "Please type in your login and try again."

                LoginAndPasswordWereIncorrect ->
                    "Sorry, your credentials could not be verified."

                NeitherLoginNorPasswordWereTypedIn ->
                    "Please type in both, your login and your password, and try again."

                NetworkError ->
                    "There was a problem connecting to the server."

                PasswordWasNotTypedIn ->
                    "Please type in your password and try again."

                EmailWasInvalid ->
                    "Please type in a valid email address in the login."

                UserWasDisabled ->
                    "Your account has been disabled."

                LoginWasNotFound ->
                    "There's no user with that email address."

                PasswordWasWrong ->
                    "You typed in the wrong password."

                UnknownError ->
                    "An unknown error occurred trying to authenticate. Please check your internet connection and credentials and try again."

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

        CreateOrEditAMeeting meeting ->
            viewCreateAMeeting


viewCreateAMeeting : Html Msg
viewCreateAMeeting =
    div [] [ text "Create a meeting" ]


viewMeetings : Html Msg
viewMeetings =
    div [] [ text "Meetings" ]


viewMenu : Html Msg
viewMenu =
    div
        []
        [ text "Menu"
        , button [ onClick MenuMeetingsButtonClicked ] [ text "Meetings" ]
        , button [ onClick MenuSignOutButtonClicked ] [ text "Sign out" ]
        ]


viewLogin : LoginModel -> Html Msg
viewLogin loginModel =
    let
        errorMessage =
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    getErrorTextUsingReasonAuthenticationFailed credentialsInputModel.reasonAuthenticationFailed

                _ ->
                    ""
    in
        case loginModel of
            CredentialsInput credentialsInputModel ->
                div
                    []
                    [ text "Login"
                    , input [ onInput LoginLoginTypedIn, type_ "text", value credentialsInputModel.login ] []
                    , input [ onInput LoginPasswordTypedIn, type_ "password", value credentialsInputModel.password ] []
                    , div [] [ errorMessage |> text ]
                    , button [ onClick LoginLoginButtonClicked ] [ text "Login" ]
                    ]

            Verification ->
                div []
                    [ text "Verifying..." ]
