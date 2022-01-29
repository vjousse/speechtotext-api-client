module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , User
    , init
    , subscriptions
    , update
    )

import Domain.Config exposing (Config)
import Domain.User
import Gen.Route
import Http
import Json.Decode as Json
import Request exposing (Request)
import Requests.User
import Storage exposing (Storage)
import Time


type alias Flags =
    Json.Value


type alias User =
    Domain.User.User


type alias Token =
    String


type alias Model =
    { storage : Storage
    , config : Config
    }


type Msg
    = SignOut
    | StorageUpdated Storage
    | GotToken (Result Http.Error Domain.User.Token)
    | RefreshToken Time.Posix


init : Request -> Flags -> ( Model, Cmd Msg )
init req flags =
    let
        config =
            flags
                |> Json.decodeValue Domain.Config.decoder
                |> Result.withDefault { serverUrl = "http://default_url", limit = 10 }

        model =
            { storage = Storage.fromJson flags
            , config = { serverUrl = config.serverUrl, limit = 10 }
            }
    in
    ( model
    , if model.storage.user /= Nothing && req.route == Gen.Route.SignIn then
        Request.replaceRoute Gen.Route.SignIn req

      else
        case model.storage.user of
            Nothing ->
                Cmd.none

            -- If there is an user in the storage, let's first check if we
            -- can get information from the server. It will allow us to
            -- validate that the token stored in the storage is still valid
            Just user ->
                Requests.User.refreshToken model.config.serverUrl user.token GotToken
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        GotToken result ->
            case result of
                -- Everything's fine, no need to logout, put the new token
                -- in session
                Ok token ->
                    ( model
                    , case model.storage.user of
                        Just user ->
                            Storage.signIn { user | token = token.access_token } model.storage

                        Nothing ->
                            Cmd.none
                    )

                -- If we're not able to get the current user we will assume it's
                -- because we are getting a 401 or something along the lines,
                -- so we redirect to the homepage
                Err e ->
                    ( model, Request.replaceRoute Gen.Route.SignIn req )

        RefreshToken _ ->
            ( model
            , case model.storage.user of
                Just user ->
                    Requests.User.refreshToken model.config.serverUrl user.token GotToken

                Nothing ->
                    Cmd.none
            )

        SignOut ->
            ( model
            , Cmd.batch
                [ Storage.signOut model.storage
                , Request.pushRoute Gen.Route.Home_ req
                ]
            )

        StorageUpdated storage ->
            ( { model | storage = storage }
            , if Gen.Route.SignIn == req.route then
                Request.pushRoute Gen.Route.Home_ req

              else
                Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Storage.load StorageUpdated
        , -- Every minute
          Time.every (1000 * 60) RefreshToken
        ]
