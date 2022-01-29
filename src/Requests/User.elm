module Requests.User exposing (getToken, getUser, refreshToken)

import Domain.Config exposing (ServerUrl)
import Domain.User
import Http
import Json.Encode as JE


getToken : ServerUrl -> String -> String -> (Result Http.Error Domain.User.Token -> msg) -> Cmd msg
getToken serverUrl login password message =
    Http.request
        { method = "POST"
        , headers = []
        , url = serverUrl ++ "/auth/jwt/login"
        , body =
            Http.multipartBody
                [ Http.stringPart "username" login
                , Http.stringPart "password" password
                ]
        , expect = Http.expectJson message Domain.User.tokenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getUser : ServerUrl -> String -> (Result Http.Error Domain.User.ApiUser -> msg) -> Cmd msg
getUser serverUrl token message =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = serverUrl ++ "/users/me"
        , body = Http.emptyBody
        , expect = Http.expectJson message Domain.User.apiUserDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


refreshToken : ServerUrl -> String -> (Result Http.Error Domain.User.Token -> msg) -> Cmd msg
refreshToken serverUrl token message =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = serverUrl ++ "/auth/jwt/refresh"
        , body = Http.emptyBody
        , expect = Http.expectJson message Domain.User.tokenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
