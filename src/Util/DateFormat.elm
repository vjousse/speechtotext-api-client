module Util.DateFormat exposing (formatDateUtc)

import DateFormat
import Time exposing (Posix)


formatDateUtc : Posix -> String
formatDateUtc time =
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.yearNumber
        , DateFormat.text " "
        , DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        , DateFormat.text " UTC"
        ]
        Time.utc
        time
