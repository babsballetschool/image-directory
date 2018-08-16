module ImageDirectory exposing (..)

import Html


main =
    Html.text "Hello, World!"



-- Model


type Entry
    = File String
    | Directory (List Entry)
