module Pages.SignIn exposing (Model, Msg(..), page)

import Domain.Config exposing (Config)
import Domain.Notification as DN exposing (Notification)
import Domain.User
import Effect
import Gen.Params.SignIn exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page
import Request
import Requests.User
import Requests.Util exposing (errorToString)
import Shared
import Storage exposing (Storage)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update shared.storage
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { login : String
    , password : String
    , notification : Maybe Notification
    , config : Config
    }


init : Shared.Model -> ( Model, Cmd Msg )
init sharedModel =
    ( { login = ""
      , password = ""
      , notification = Nothing
      , config = sharedModel.config
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LoginSubmitted
    | GotToken (Result Http.Error Domain.User.Token)
    | LoginInput String
    | PasswordInput String
    | ClearNotification


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        ClearNotification ->
            ( { model | notification = Nothing }, Cmd.none )

        -- Sign user when we get the Token
        GotToken result ->
            case result of
                Ok token ->
                    ( model
                    , Storage.signIn { email = model.login, token = token.access_token } storage
                    )

                Err e ->
                    ( { model
                        | notification =
                            Just
                                { title = "Http Error"
                                , subtitle = Just (Requests.Util.errorToString e)
                                , type_ = DN.Error
                                }
                      }
                    , Cmd.none
                    )

        LoginInput login ->
            ( { model | login = login }, Cmd.none )

        LoginSubmitted ->
            ( model, Requests.User.getToken model.config.serverUrl model.login model.password GotToken )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        UI.guestLayout
            [ div
                [ class "sm:mx-auto sm:w-full sm:max-w-md"
                ]
                [ img
                    [ class "mx-auto h-12 w-auto"
                    , src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                    , alt "Workflow"
                    ]
                    []
                , h2
                    [ class "mt-6 text-center text-3xl font-extrabold text-gray-900"
                    ]
                    [ text "Sign in to your account" ]
                ]
            , div
                [ class "mt-8 sm:mx-auto sm:w-full sm:max-w-md"
                ]
                [ div
                    [ class "bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10"
                    ]
                    [ Html.div
                        [ class "space-y-6"
                        ]
                        [ div []
                            [ label
                                [ for "email"
                                , class "block text-sm font-medium text-gray-700"
                                ]
                                [ text "Email address" ]
                            , div
                                [ class "mt-1"
                                ]
                                [ input
                                    [ placeholder "Email"
                                    , type_ "text"
                                    , value model.login
                                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                    , onInput LoginInput
                                    , required True
                                    ]
                                    []
                                ]
                            ]
                        , div []
                            [ label
                                [ for "password"
                                , class "block text-sm font-medium text-gray-700"
                                ]
                                [ text "Password" ]
                            , div
                                [ class "mt-1"
                                ]
                                [ input
                                    [ placeholder "Password"
                                    , type_ "password"
                                    , value model.password
                                    , class "appearance-none block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm placeholder-gray-400 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                                    , onInput PasswordInput
                                    , required True
                                    ]
                                    []
                                ]
                            ]
                        , div []
                            [ button
                                [ type_ "submit"
                                , class "w-full flex justify-center py-2 px-4 border border-transparent rounded-md shadow-sm text-sm font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                , onClick LoginSubmitted
                                ]
                                [ text "Sign in" ]
                            ]
                        ]
                    ]
                ]
            , UI.displayNotification model.notification ClearNotification
            ]
    }
