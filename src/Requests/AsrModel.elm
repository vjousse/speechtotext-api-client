module Requests.AsrModel exposing (get)

import Auth exposing (User)
import Domain.AsrModel exposing (AsrModel)
import Domain.Config exposing (ServerUrl)
import Http


get : ServerUrl -> User -> (Result Http.Error (List AsrModel) -> msg) -> Cmd msg
get serverUrl user message =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ user.token ]
        , url = serverUrl ++ "/asr_models/"
        , body = Http.emptyBody
        , expect = Http.expectJson message Domain.AsrModel.listDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
