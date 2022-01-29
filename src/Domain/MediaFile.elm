module Domain.MediaFile exposing (MediaFile, decoder, encode)

import Domain.Result as R exposing (Result)
import Domain.Task as T exposing (Task)
import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time


type alias MediaFile =
    { id : Int
    , filename : String
    , createdAt : Time.Posix
    , uuid : String
    , tasks : List Task
    , results : List Result
    }


decoder : Json.Decoder MediaFile
decoder =
    Json.map6 MediaFile
        (Json.field "id" Json.int)
        (Json.field "filename" Json.string)
        (Json.field "created_at" Iso8601.decoder)
        (Json.field "uuid" Json.string)
        (Json.field "tasks" (Json.list T.decoder))
        (Json.field "results" (Json.list R.decoder))


encode : MediaFile -> Json.Value
encode mf =
    Encode.object
        [ ( "id", Encode.int mf.id )
        , ( "filename", Encode.string mf.filename )
        , ( "created_at", Iso8601.encode mf.createdAt )
        , ( "uuid", Encode.string mf.uuid )
        , ( "tasks", Encode.list T.encode mf.tasks )
        , ( "results", Encode.list R.encode mf.results )
        ]
