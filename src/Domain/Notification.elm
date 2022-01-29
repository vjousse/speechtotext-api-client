module Domain.Notification exposing (Notification, NotificationType(..))

import Http


type NotificationType
    = Success
    | Error


type alias Notification =
    { title : String
    , subtitle : Maybe String
    , type_ : NotificationType
    }
