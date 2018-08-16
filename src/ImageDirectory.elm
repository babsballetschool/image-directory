module ImageDirectory exposing (..)

import Html
import Json.Encode as Encode


main =
    let
        value = encode entry

        text =
            Encode.encode 4 value
    in
        Html.text text


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



-- Encode


encode : Entry -> Encode.Value
encode entry =
    case entry of
        File location ->
            Encode.object
                [ ( "location", Encode.string location )
                ]

        Directory entries ->
            entries
                |> List.map encode
                |> Encode.list
