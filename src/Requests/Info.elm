module Requests.Info exposing (get)

import Auth exposing (User)
import Domain.Config exposing (ServerUrl)
import Domain.Info exposing (Info)
import Http


get : ServerUrl -> User -> (Result Http.Error Info -> msg) -> Cmd msg
get serverUrl user message =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ user.token ]
        , url = serverUrl ++ "/infos"
        , body = Http.emptyBody
        , expect = Http.expectJson message Domain.Info.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
