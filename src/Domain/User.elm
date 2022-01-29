module Domain.User exposing (ApiUser, Token, User, apiUserDecoder, decoder, encode, tokenDecoder)

import Json.Decode as Json
import Json.Encode as Encode


type alias User =
    { email : String
    , token : String
    }


type alias Token =
    { access_token : String
    , token_type : String
    }


type alias ApiUser =
    { id : String
    , email : String
    , is_active : Bool
    , is_superuser : Bool
    }


decoder : Json.Decoder User
decoder =
    Json.map2 User
        (Json.field "email" Json.string)
        (Json.field "token" Json.string)


encode : User -> Json.Value
encode user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        ]


tokenDecoder : Json.Decoder Token
tokenDecoder =
    Json.map2 Token
        (Json.field "access_token" Json.string)
        (Json.field "token_type" Json.string)


apiUserDecoder : Json.Decoder ApiUser
apiUserDecoder =
    Json.map4 ApiUser
        (Json.field "id" Json.string)
        (Json.field "email" Json.string)
        (Json.field "is_active" Json.bool)
        (Json.field "is_superuser" Json.bool)
