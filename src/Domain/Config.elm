module Domain.Config exposing (Config, ServerUrl, decoder, encode)

import Json.Decode as Json
import Json.Encode as Encode


type alias ServerUrl =
    String


type alias Config =
    { serverUrl : ServerUrl
    , limit : Int
    }


decoder : Json.Decoder Config
decoder =
    Json.map2 Config
        (Json.field "server_url" Json.string)
        (Json.succeed 10)


encode : Config -> Json.Value
encode config =
    Encode.object
        [ ( "server_url", Encode.string config.serverUrl )
        , ( "limit", Encode.int config.limit )
        ]
