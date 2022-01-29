module Domain.Info exposing (Info, decoder, encode)

import Json.Decode as Json
import Json.Encode as Encode


type alias Info =
    { allowedMimetypes : List String
    }


decoder : Json.Decoder Info
decoder =
    Json.map Info
        (Json.field "allowed_mimetypes" (Json.list Json.string))


encode : Info -> Json.Value
encode info =
    Encode.object
        [ ( "allowed_mimetypes", Encode.list Encode.string info.allowedMimetypes )
        ]
