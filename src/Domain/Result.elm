module Domain.Result exposing (Result, decoder, encode)

import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time


type alias Result =
    { id : Int
    , filename : String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Json.Decoder Result
decoder =
    Json.map4 Result
        (Json.field "id" Json.int)
        (Json.field "filename" Json.string)
        (Json.field "created_at" Iso8601.decoder)
        (Json.field "updated_at" Iso8601.decoder)


encode : Result -> Json.Value
encode result =
    Encode.object
        [ ( "id", Encode.int result.id )
        , ( "filename", Encode.string result.filename )
        , ( "created_at", Iso8601.encode result.createdAt )
        , ( "updated_at", Iso8601.encode result.createdAt )
        ]
