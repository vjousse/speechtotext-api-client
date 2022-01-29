module Domain.MediaFileList exposing (MediaFileList, decoder, encode)

import Domain.MediaFile exposing (MediaFile)
import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time


type alias MediaFileList =
    { totalCount : Int
    , offset : Int
    , limit : Int
    , files : List MediaFile
    }


decoder : Json.Decoder MediaFileList
decoder =
    Json.map4 MediaFileList
        (Json.field "total_count" Json.int)
        (Json.field "offset" Json.int)
        (Json.field "limit" Json.int)
        (Json.field "files" (Json.list Domain.MediaFile.decoder))


encode : MediaFileList -> Json.Value
encode mfl =
    Encode.object
        [ ( "total_count", Encode.int mfl.totalCount )
        , ( "offset", Encode.int mfl.offset )
        , ( "limit", Encode.int mfl.limit )
        , ( "files", Encode.list Domain.MediaFile.encode mfl.files )
        ]
