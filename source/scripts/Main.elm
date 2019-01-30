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



--INIT


type alias CredentialsToVerify =
    { login : String
    , password : String
    }


type alias MaybeLogin =
    Maybe String


type alias MaybePassword =
    Maybe String


type alias Meeting =
    { name : String
    , date : String
    , scheduledEndTime : String
    , scheduledStartTime : String
    }


type alias Meetings =
    List Meeting


type alias LoginModel =
    { login : Maybe String
    , password : Maybe String
    , reasonWhyAuthenticationFailed : Maybe ReasonWhyAuthenticationFailed
    }


type alias User =
    { name : String }


type Model
    = Ready
    | CredentialsAreBeingTypedIn LoginModel
    | VerifyingCredentials
    | FailedToVerifyCredentials LoginModel
    | ShowMainMenu User
    | ShowMeetings User Meetings
    | CreateAMeeting User Meeting


type ReasonWhyAuthenticationFailed
    = LoginHasntBeenTypedIn
    | PasswordHasntBeenTypedIn
    | LoginAndPasswordHasntBeenTypedIn
    | LoginAndPasswordCombinationDoesntExistInTheDatabase
    | NetworkIsDisconnected


init : () -> ( Model, Cmd Msg )
init _ =
    ( Ready, Cmd.none )



--UPDATE


type Msg
    = NoOp
    | LoginButtonClicked
    | LoginIsBeingTypedIn String
    | PasswordIsBeingTypedIn String


setLoginModelsReasonWhyAuthenticationFailed : LoginModel -> ReasonWhyAuthenticationFailed -> Model
setLoginModelsReasonWhyAuthenticationFailed loginModel reasonWhyAuthenticationFailed =
    let
        newLoginModel =
            { loginModel | reasonWhyAuthenticationFailed = Just reasonWhyAuthenticationFailed }
    in
        FailedToVerifyCredentials newLoginModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoginButtonClicked ->
            let
                newModelCommand =
                    case model of
                        Ready ->
                            let
                                newLoginModel =
                                    { login = Nothing
                                    , password = Nothing
                                    , reasonWhyAuthenticationFailed = Just LoginAndPasswordHasntBeenTypedIn
                                    }
                            in
                                ( FailedToVerifyCredentials newLoginModel, Cmd.none )

                        CredentialsAreBeingTypedIn loginModel ->
                            case ( loginModel.login, loginModel.password ) of
                                ( Just login, Just password ) ->
                                    let
                                        credentialsToVerify =
                                            CredentialsToVerify login password
                                    in
                                        ( VerifyingCredentials, verifyCredentials credentialsToVerify )

                                ( Just login, Nothing ) ->
                                    ( setLoginModelsReasonWhyAuthenticationFailed loginModel PasswordHasntBeenTypedIn, Cmd.none )

                                ( Nothing, Just password ) ->
                                    ( setLoginModelsReasonWhyAuthenticationFailed loginModel LoginHasntBeenTypedIn, Cmd.none )

                                ( Nothing, Nothing ) ->
                                    ( setLoginModelsReasonWhyAuthenticationFailed loginModel LoginAndPasswordHasntBeenTypedIn, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
                newModelCommand

        LoginIsBeingTypedIn login ->
            let
                newModel =
                    case model of
                        Ready ->
                            let
                                newLoginModel =
                                    { login = Just login
                                    , password = Nothing
                                    , reasonWhyAuthenticationFailed = Nothing
                                    }
                            in
                                CredentialsAreBeingTypedIn newLoginModel

                        CredentialsAreBeingTypedIn loginModel ->
                            let
                                newLoginModel =
                                    { loginModel | login = Just login }
                            in
                                CredentialsAreBeingTypedIn newLoginModel

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        PasswordIsBeingTypedIn password ->
            let
                newModel =
                    case model of
                        Ready ->
                            let
                                newLoginModel =
                                    { login = Nothing
                                    , password = Just password
                                    , reasonWhyAuthenticationFailed = Nothing
                                    }
                            in
                                CredentialsAreBeingTypedIn newLoginModel

                        CredentialsAreBeingTypedIn loginModel ->
                            let
                                newLoginModel =
                                    { loginModel | password = Just password }
                            in
                                CredentialsAreBeingTypedIn newLoginModel

                        _ ->
                            model
            in
                ( newModel, Cmd.none )


port verifyCredentials : CredentialsToVerify -> Cmd msg



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


view : Model -> Html Msg
view model =
    case model of
        Ready ->
            viewLogin model

        CredentialsAreBeingTypedIn loginModel ->
            viewLogin model

        VerifyingCredentials ->
            viewVerifyingCredentials

        FailedToVerifyCredentials loginModel ->
            viewLogin model

        ShowMainMenu user ->
            viewMainMenu user

        ShowMeetings user meetings ->
            viewMeetings user meetings

        CreateAMeeting user meeting ->
            viewCreateAMeeting user


viewCreateAMeeting : User -> Html Msg
viewCreateAMeeting user =
    div [] [ text "Create a meeting" ]


viewLogin : Model -> Html Msg
viewLogin model =
    div []
        [ input [ onInput LoginIsBeingTypedIn ] []
        , input [ onInput PasswordIsBeingTypedIn, type_ "password" ] []
        , button [ onClick LoginButtonClicked ] [ text "Login" ]
        ]


viewMainMenu : User -> Html Msg
viewMainMenu user =
    div []
        [ button [] [ text "Meetings" ]
        , button [] [ text "Sign out" ]
        ]


viewMeeting : Meeting -> Html Msg
viewMeeting meeting =
    div [] [ text meeting.name ]


viewMeetings : User -> Meetings -> Html Msg
viewMeetings user meetings =
    div [] ([ text "Meetings" ] ++ (List.map viewMeeting meetings))


viewVerifyingCredentials : Html Msg
viewVerifyingCredentials =
    div [] [ text "Verifying credentials" ]
