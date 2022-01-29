module Requests.Upload exposing (upload)

import Auth exposing (User)
import Domain.Config exposing (ServerUrl)
import File exposing (File)
import Http


upload : ServerUrl -> List File.File -> Int -> User -> (Result Http.Error () -> msg) -> Cmd msg
upload serverUrl files modelId user message =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ user.token ]
        , url = serverUrl ++ "/files/upload/"
        , body =
            --Http.multipartBody
            --    [ Http.stringPart "media_file:duration" "2"
            --    , Http.stringPart "media_file:size" "3"
            --    , Http.filePart "media_file:file_upload" file
            --    ]
            Http.multipartBody
                ([ Http.stringPart "asr_model_id" <| String.fromInt modelId
                 ]
                    ++ List.map (Http.filePart "files") files
                )
        , expect = Http.expectWhatever message
        , timeout = Nothing
        , tracker = Nothing
        }
