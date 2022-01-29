module Domain.Task exposing (Task, decoder, encode)

import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time


type alias Task =
    { id : String
    , actorName : String
    , queueName : String
    , status : String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Json.Decoder Task
decoder =
    Json.map6 Task
        (Json.field "id" Json.string)
        (Json.field "actor_name" Json.string)
        (Json.field "queue_name" Json.string)
        (Json.field "status" Json.string)
        (Json.field "created_at" Iso8601.decoder)
        (Json.field "updated_at" Iso8601.decoder)


encode : Task -> Json.Value
encode task =
    Encode.object
        [ ( "id", Encode.string task.id )
        , ( "actor_name", Encode.string task.actorName )
        , ( "queue_name", Encode.string task.queueName )
        , ( "status", Encode.string task.status )
        , ( "created_at", Iso8601.encode task.createdAt )
        , ( "updated_at", Iso8601.encode task.createdAt )
        ]
