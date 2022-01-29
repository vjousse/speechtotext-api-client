module Requests.MediaFile exposing (getAll)

import Auth exposing (User)
import Domain.Config exposing (ServerUrl)
import Domain.MediaFileList exposing (MediaFileList)
import Http
import Json.Decode as Json


getAll : ServerUrl -> User -> Int -> Int -> (Result Http.Error MediaFileList -> msg) -> Cmd msg
getAll serverUrl user offset limit message =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ user.token ]
        , url = serverUrl ++ "/files/?offset=" ++ String.fromInt offset ++ "&limit=" ++ String.fromInt limit
        , body = Http.emptyBody
        , expect = Http.expectJson message Domain.MediaFileList.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
