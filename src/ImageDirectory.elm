module ImageDirectory exposing (..)

import Html


main =
    Html.text "Hello, World!"


entry : Entry
entry =
    Directory
        [ File "A"
        , File "B"
        , Directory
            [ File "D/A"
            , File "D/B"
            ]
        ]



-- Model


type Entry
    = File String
    | Directory (List Entry)
