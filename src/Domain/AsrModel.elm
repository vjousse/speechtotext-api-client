module Domain.AsrModel exposing (AsrModel, decoder, encode, listDecoder)

import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time


type alias AsrModel =
    { id : Int
    , label : String
    , description : String
    , lang : String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Json.Decoder AsrModel
decoder =
    Json.map6 AsrModel
        (Json.field "id" Json.int)
        (Json.field "label" Json.string)
        (Json.field "description" Json.string)
        (Json.field "lang" Json.string)
        (Json.field "created_at" Iso8601.decoder)
        (Json.field "updated_at" Iso8601.decoder)


listDecoder : Json.Decoder (List AsrModel)
listDecoder =
    Json.list decoder


encode : AsrModel -> Json.Value
encode asrModel =
    Encode.object
        [ ( "id", Encode.int asrModel.id )
        , ( "label", Encode.string asrModel.label )
        , ( "description", Encode.string asrModel.description )
        , ( "lang", Encode.string asrModel.lang )
        , ( "created_at", Iso8601.encode asrModel.createdAt )
        , ( "updated_at", Iso8601.encode asrModel.createdAt )
        ]
