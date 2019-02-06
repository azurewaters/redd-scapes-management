port module Main exposing (..)

import Browser exposing (element)
import Debug exposing (..)
import Dict exposing (Dict)
import Time
import Html exposing (button, div, Html, img, input, label, section, text)
import Html.Attributes exposing (autofocus, class, classList, id, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (bool, Decoder, decodeValue, field, int, string, Value)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias CreateOrEditAMeetingModel =
    { guests : Guests
    , meeting : Meeting
    , newGuestsName : String
    , newGuestsEmail : String
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


type alias Guest =
    { email : String
    , id : String
    , name : String
    }


type alias Guests =
    Dict Int Guest


type alias Meeting =
    { id : String
    , name : String
    , date : Milliseconds
    , startTime : Int
    , endTime : Int
    }


type alias MeetingsIsShowingModel =
    List Meeting


type alias Milliseconds =
    Int


type LoginIsShowingModel
    = CredentialsInput CredentialsInputModel
    | Verification


type Model
    = LoginIsShowing LoginIsShowingModel
    | MeetingsIsShowing MeetingsIsShowingModel
    | CreateOrEditAMeetingIsShowing CreateOrEditAMeetingModel


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


emptyCreateOrEditAMeetingModel : CreateOrEditAMeetingModel
emptyCreateOrEditAMeetingModel =
    { guests = Dict.empty
    , meeting = emptyMeeting
    , newGuestsName = ""
    , newGuestsEmail = ""
    }


emptyCredentialsInputModel : CredentialsInputModel
emptyCredentialsInputModel =
    { login = ""
    , password = ""
    , reasonAuthenticationFailed = Nothing
    }


emptyGuests : Guests
emptyGuests =
    Dict.fromList []


emptyLoginModel : LoginIsShowingModel
emptyLoginModel =
    CredentialsInput emptyCredentialsInputModel


emptyMeeting : Meeting
emptyMeeting =
    { id = ""
    , name = ""
    , date = 0
    , startTime = 0
    , endTime = 0
    }


emptyMeetingsModel : MeetingsIsShowingModel
emptyMeetingsModel =
    []


emptyModel : Model
emptyModel =
    LoginIsShowing emptyLoginModel


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = AuthenticatedUserSignedOut
    | AuthenticationFailed String
    | AuthenticationSucceeded
    | CreateOrEditAMeetingAddANewGuestClicked
    | CreateOrEditAMeetingCloseButtonClicked
    | CreateOrEditAMeetingNewGuestsEmailTypedIn String
    | CreateOrEditAMeetingNewGuestsNameTypedIn String
    | CreateOrEditAMeetingNewMeetingsNameTypedIn String
    | LoginIsShowingLoginButtonClicked
    | LoginIsShowingLoginTypedIn String
    | LoginIsShowingPasswordTypedIn String
    | MeetingsIsShowingMeetingItemEditButtonClicked Meeting
    | MeetingsIsShowingMeetingItemDeleteButtonClicked String
    | MeetingsIsShowingMeetingWasDeleted String
    | MeetingsIsShowingSignOutButtonClicked
    | MeetingsWasUpdated (List Meeting)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        CreateOrEditAMeetingIsShowing createOrEditAMeetingModel ->
            case msg of
                CreateOrEditAMeetingAddANewGuestClicked ->
                    if
                        (String.isEmpty createOrEditAMeetingModel.newGuestsName |> not)
                            && (String.isEmpty createOrEditAMeetingModel.newGuestsEmail |> not)
                    then
                        ( CreateOrEditAMeetingIsShowing
                            { createOrEditAMeetingModel
                                | guests = createANewGuestAndAddItToADictionaryOfGuests createOrEditAMeetingModel.guests createOrEditAMeetingModel.newGuestsName createOrEditAMeetingModel.newGuestsEmail
                                , newGuestsName = ""
                                , newGuestsEmail = ""
                            }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                CreateOrEditAMeetingCloseButtonClicked ->
                    ( MeetingsIsShowing emptyMeetingsModel, Cmd.none )

                CreateOrEditAMeetingNewGuestsEmailTypedIn newNewGuestsEmail ->
                    ( CreateOrEditAMeetingIsShowing { createOrEditAMeetingModel | newGuestsEmail = newNewGuestsEmail }, Cmd.none )

                CreateOrEditAMeetingNewGuestsNameTypedIn newNewGuestsName ->
                    ( CreateOrEditAMeetingIsShowing { createOrEditAMeetingModel | newGuestsName = newNewGuestsName }, Cmd.none )

                CreateOrEditAMeetingNewMeetingsNameTypedIn newName ->
                    let
                        changeMeetingsName : CreateOrEditAMeetingModel -> String -> CreateOrEditAMeetingModel
                        changeMeetingsName existingCreateOrEditAMeetingModel meetingsNewName =
                            let
                                existingMeeting =
                                    existingCreateOrEditAMeetingModel.meeting
                            in
                                { existingCreateOrEditAMeetingModel | meeting = { existingMeeting | name = meetingsNewName } }
                    in
                        ( CreateOrEditAMeetingIsShowing (changeMeetingsName createOrEditAMeetingModel newName), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoginIsShowing loginIsShowingModel ->
            case loginIsShowingModel of
                CredentialsInput credentialsInputModel ->
                    case msg of
                        AuthenticationSucceeded ->
                            ( MeetingsIsShowing [], fetchMeetings True )

                        LoginIsShowingLoginTypedIn login ->
                            ( updateLoginTypedIn model login, Cmd.none )

                        LoginIsShowingPasswordTypedIn password ->
                            ( updatePasswordTypedIn model password, Cmd.none )

                        LoginIsShowingLoginButtonClicked ->
                            ( LoginIsShowing (updateLoginButtonClicked loginIsShowingModel)
                            , updateLoginButtonClickedCommand loginIsShowingModel
                            )

                        _ ->
                            ( model, Cmd.none )

                Verification ->
                    case msg of
                        AuthenticationFailed authenticationFailedErrorCode ->
                            ( { login = ""
                              , password = ""
                              , reasonAuthenticationFailed = getReasonAuthenticationFailedUsingAuthenticationFailedErrorCode authenticationFailedErrorCode
                              }
                                |> CredentialsInput
                                |> LoginIsShowing
                            , Cmd.none
                            )

                        AuthenticationSucceeded ->
                            ( MeetingsIsShowing [], fetchMeetings True )

                        _ ->
                            ( model, Cmd.none )

        MeetingsIsShowing meetingsIsShowingModel ->
            case msg of
                AuthenticatedUserSignedOut ->
                    ( LoginIsShowing emptyLoginModel, Cmd.none )

                MeetingsIsShowingMeetingItemDeleteButtonClicked meetingId ->
                    ( model, deleteThisMeeting meetingId )

                MeetingsIsShowingMeetingItemEditButtonClicked meeting ->
                    ( CreateOrEditAMeetingIsShowing { emptyCreateOrEditAMeetingModel | meeting = meeting }, fetchAMeetingsGuests meeting.id )

                MeetingsIsShowingSignOutButtonClicked ->
                    ( model, signOut True )

                MeetingsIsShowingMeetingWasDeleted meetingId ->
                    let
                        newMeetingsIsShowingModel =
                            meetingsIsShowingModel
                                |> List.filter (\meeting -> meeting.id /= meetingId)
                    in
                        ( MeetingsIsShowing newMeetingsIsShowingModel, Cmd.none )

                MeetingsWasUpdated meetings ->
                    case model of
                        MeetingsIsShowing meetingsModel ->
                            ( MeetingsIsShowing meetings, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateLoginButtonClicked : LoginIsShowingModel -> LoginIsShowingModel
updateLoginButtonClicked loginIsShowingModel =
    case loginIsShowingModel of
        CredentialsInput credentialsInputModel ->
            let
                reasonAuthenticationFailed =
                    getReasonAuthenticationFailedUsingLoginAndPassword credentialsInputModel.login credentialsInputModel.password
            in
                if reasonAuthenticationFailed == Nothing then
                    Verification
                else
                    { credentialsInputModel | reasonAuthenticationFailed = reasonAuthenticationFailed }
                        |> CredentialsInput

        _ ->
            loginIsShowingModel


updateLoginButtonClickedCommand : LoginIsShowingModel -> Cmd Msg
updateLoginButtonClickedCommand loginIsShowingModel =
    case loginIsShowingModel of
        CredentialsInput credentialsInputModel ->
            if (credentialsInputModel.login /= "") && (credentialsInputModel.password /= "") then
                verifyCredentials { login = credentialsInputModel.login, password = credentialsInputModel.password }
            else
                Cmd.none

        _ ->
            Cmd.none


updateLoginTypedIn : Model -> String -> Model
updateLoginTypedIn model login =
    case model of
        LoginIsShowing loginIsShowingModel ->
            case loginIsShowingModel of
                CredentialsInput credentialsInputModel ->
                    { credentialsInputModel | login = login }
                        |> CredentialsInput
                        |> LoginIsShowing

                _ ->
                    LoginIsShowing loginIsShowingModel

        _ ->
            model


updatePasswordTypedIn : Model -> String -> Model
updatePasswordTypedIn model password =
    case model of
        LoginIsShowing loginIsShowingModel ->
            case loginIsShowingModel of
                CredentialsInput credentialsInputModel ->
                    { credentialsInputModel | password = password }
                        |> CredentialsInput
                        |> LoginIsShowing

                _ ->
                    LoginIsShowing loginIsShowingModel

        _ ->
            model


port deleteThisMeeting : String -> Cmd msg


port fetchMeetings : Bool -> Cmd msg


port fetchAMeetingsGuests : String -> Cmd msg


port signOut : Bool -> Cmd msg


port verifyCredentials : Credentials -> Cmd msg



-- SUBSCRIPTIONS


dateDecoder : Decoder Int
dateDecoder =
    field "date" int


emailDecoder : Decoder String
emailDecoder =
    field "email" string


endTimeDecoder : Decoder Int
endTimeDecoder =
    field "endTime" int


guestDecoder : Decoder Guest
guestDecoder =
    Decode.map3 Guest idDecoder nameDecoder emailDecoder


guestsDecoder : Decoder (List Guest)
guestsDecoder =
    field "guests" (Decode.list guestDecoder)


idDecoder : Decoder String
idDecoder =
    field "id" string


meetingDecoder : Decoder Meeting
meetingDecoder =
    Decode.map5 Meeting idDecoder nameDecoder dateDecoder startTimeDecoder endTimeDecoder


nameDecoder : Decoder String
nameDecoder =
    field "name" string


startTimeDecoder : Decoder Int
startTimeDecoder =
    field "startTime" int


uidDecoder : Decoder String
uidDecoder =
    field "uid" string


decodeAuthenticationFailedFromJS : Value -> Msg
decodeAuthenticationFailedFromJS receivedValue =
    case decodeValue string receivedValue of
        Ok errorCode ->
            AuthenticationFailed errorCode

        Err error ->
            Debug.log (Debug.toString error) NoOp


decodeAuthenticatedUserFromJS : Value -> Msg
decodeAuthenticatedUserFromJS receivedValue =
    case decodeValue bool receivedValue of
        Ok value ->
            if value then
                AuthenticationSucceeded
            else
                {- TODO: Not so sure about this -}
                AuthenticatedUserSignedOut

        Err error ->
            Debug.log (Debug.toString error) AuthenticatedUserSignedOut


decodeAuthenticatedUserSignedOutFromJS : Value -> Msg
decodeAuthenticatedUserSignedOutFromJS receivedValue =
    case decodeValue bool receivedValue of
        Ok authenticationFailed ->
            if authenticationFailed then
                AuthenticatedUserSignedOut
            else
                NoOp

        Err error ->
            Debug.log (Debug.toString error) NoOp


decodeMeetingsFromJS : Value -> Msg
decodeMeetingsFromJS receivedValue =
    case decodeValue (Decode.list meetingDecoder) receivedValue of
        Ok meetings ->
            MeetingsWasUpdated meetings

        Err error ->
            Debug.log (Debug.toString error) NoOp


decodeMeetingWasDeletedFromJS : Value -> Msg
decodeMeetingWasDeletedFromJS receivedValue =
    case decodeValue Decode.string receivedValue of
        Ok meetingId ->
            MeetingsIsShowingMeetingWasDeleted meetingId

        Err error ->
            Debug.log (Debug.toString error) NoOp


port authenticationFailedFromJS : (Value -> msg) -> Sub msg


port authenticationSucceededFromJS : (Value -> msg) -> Sub msg


port authenticatedUserSignedOutFromJS : (Value -> msg) -> Sub msg


port meetingsFromJS : (Value -> msg) -> Sub msg


port meetingWasDeletedFromJS : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authenticationFailedFromJS decodeAuthenticationFailedFromJS
        , authenticationSucceededFromJS decodeAuthenticatedUserFromJS
        , authenticatedUserSignedOutFromJS decodeAuthenticatedUserSignedOutFromJS
        , meetingsFromJS decodeMeetingsFromJS
        , meetingWasDeletedFromJS decodeMeetingWasDeletedFromJS
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        LoginIsShowing loginModel ->
            viewLogin loginModel

        MeetingsIsShowing meetingsModel ->
            viewMeetings meetingsModel

        CreateOrEditAMeetingIsShowing createOrEditAMeetingModel ->
            viewCreateOrEditAMeetingIsShowing createOrEditAMeetingModel


viewCreateOrEditAMeetingIsShowing : CreateOrEditAMeetingModel -> Html Msg
viewCreateOrEditAMeetingIsShowing createOrEditAMeetingModel =
    section
        [ id "createOrEditAMeeting" ]
        [ section [ class "navigation" ] [ label [ classList [ ( "yellowTextButton", True ), ( "closeButton", True ) ], id "closeButton", onClick CreateOrEditAMeetingCloseButtonClicked ] [ text "+" ] ]
        , section
            [ id "createANewMeetingPanel" ]
            [ div
                [ class "labelledInput" ]
                [ label [ class "labelForInput" ] [ text "Name of the Meeting" ]
                , input [ autofocus True, onInput CreateOrEditAMeetingNewMeetingsNameTypedIn, type_ "text", value createOrEditAMeetingModel.meeting.name ] []
                ]
            , div [ id "timeAndDateHolder" ] []
            , div [ id "guestsList" ] (viewGuests createOrEditAMeetingModel.guests)
            , div [ id "newGuest" ]
                [ input [ onInput CreateOrEditAMeetingNewGuestsNameTypedIn, value createOrEditAMeetingModel.newGuestsName ] []
                , input [ onInput CreateOrEditAMeetingNewGuestsEmailTypedIn, value createOrEditAMeetingModel.newGuestsEmail ] []
                , button [ onClick CreateOrEditAMeetingAddANewGuestClicked ] [ text "Add Guest" ]
                ]
            ]
        ]


viewGuest : ( Int, Guest ) -> Html Msg
viewGuest ( key, guest ) =
    div []
        [ text (key |> String.fromInt)
        , text guest.name
        , text guest.email
        ]


viewGuests : Guests -> List (Html Msg)
viewGuests guests =
    guests
        |> Dict.toList
        |> List.map viewGuest


viewLabelledInput : String -> Bool -> (String -> Msg) -> String -> Html Msg
viewLabelledInput labelForInput autofocusOn msgToEmit valueToShow =
    div
        [ class "labelledInput" ]
        [ label [ class "labelForInput" ] [ text labelForInput ]
        , input [ autofocus autofocusOn, onInput msgToEmit, type_ "text", value valueToShow ] []
        ]


viewMeetingItem : Meeting -> Html Msg
viewMeetingItem meeting =
    div
        [ class "meetingItem" ]
        [ div [] [ div [ class "labelForDisplayLabel" ] [ text "Name of the Meeting" ], div [ class "displayLabel" ] [ text meeting.name ] ]
        , div [] [ div [ class "labelForDisplayLabel" ] [ text "Date" ], div [ class "displayLabel" ] [ getDisplayDate meeting.date |> text ] ]
        , div [] [ div [ class "labelForDisplayLabel" ] [ text "Time" ], div [ class "displayLabel" ] [ meeting.startTime |> String.fromInt |> text ] ]
        , div [ class "yellowTextButton", onClick (MeetingsIsShowingMeetingItemEditButtonClicked meeting) ] [ text "Edit" ]
        , div [ class "yellowTextButton", onClick (MeetingsIsShowingMeetingItemDeleteButtonClicked meeting.id) ] [ text "Delete" ]
        ]


viewMeetings : MeetingsIsShowingModel -> Html Msg
viewMeetings meetingsModel =
    section
        [ id "meetings" ]
        [ section [ class "navigation" ] [ label [ class "yellowTextButton", onClick MeetingsIsShowingSignOutButtonClicked ] [ text "Sign out" ] ]
        , section [ id "meetingsPanel" ] [ section [ id "meetingItemsList" ] (viewMeetingItemsList meetingsModel.meetings) ]
        ]


viewMeetingItemsList : List Meeting -> List (Html Msg)
viewMeetingItemsList meetings =
    if List.isEmpty meetings then
        [ text "There are no meetings" ]
    else
        List.map viewMeetingItem meetings


viewLogin : LoginIsShowingModel -> Html Msg
viewLogin loginModel =
    let
        loginValue =
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    credentialsInputModel.login

                _ ->
                    ""

        passwordValue =
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    credentialsInputModel.password

                _ ->
                    ""

        message =
            case loginModel of
                CredentialsInput credentialsInputModel ->
                    getErrorTextUsingReasonAuthenticationFailed credentialsInputModel.reasonAuthenticationFailed

                Verification ->
                    "Verifying..."
    in
        section
            [ id "loginPanel" ]
            [ img [ id "logo", src "images/reddLogo.png" ] []
            , div
                [ class "labelledInput" ]
                [ label [ class "labelForInput" ] [ text "Login" ]
                , input [ autofocus True, onInput LoginIsShowingLoginTypedIn, type_ "text", value loginValue ] []
                ]
            , div
                [ class "labelledInput" ]
                [ label [] [ text "Password" ]
                , input [ onInput LoginIsShowingPasswordTypedIn, type_ "password", value passwordValue ] []
                ]
            , div [ class "messageDisplay" ] [ message |> text ]
            , button [ class "bigYellowRoundedButton", onClick LoginIsShowingLoginButtonClicked ] [ text "Login" ]
            ]



-- HELPER FUNCTIONS


createANewGuestAndAddItToADictionaryOfGuests : Guests -> String -> String -> Guests
createANewGuestAndAddItToADictionaryOfGuests guests newGuestsName newGuestsEmail =
    let
        newKey =
            Dict.keys guests
                |> List.maximum
                |> (\maximum ->
                        case maximum of
                            Just max ->
                                max + 1

                            Nothing ->
                                1
                   )

        newGuest : Guest
        newGuest =
            { email = newGuestsEmail
            , id = "0"
            , name = newGuestsName
            }
    in
        Dict.insert newKey newGuest guests


getLongMonthsName : Time.Month -> String
getLongMonthsName month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


getDisplayDate : Milliseconds -> String
getDisplayDate dateInMilliseconds =
    let
        date =
            Time.millisToPosix dateInMilliseconds

        year =
            Time.toYear Time.utc date

        month =
            Time.toMonth Time.utc date

        day =
            Time.toDay Time.utc date
    in
        (String.fromInt day) ++ " " ++ (getLongMonthsName month) ++ " " ++ (String.fromInt year)


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
