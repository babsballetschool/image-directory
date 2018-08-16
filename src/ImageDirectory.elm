module ImageDirectory exposing (..)

import Html
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom)


main =
    let
        value =
            encode example

        text =
            Encode.encode 4 value
    in
        Html.text text


example : Entry
example =
    let
        exampleEntry : Result String Entry
        exampleEntry =
            Decode.decodeString directoryEntry """[{"location": "b"}, {"location": "c"}]"""
    in
        Result.withDefault (File "a") exampleEntry



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



-- Decode


entry : Decode.Decoder Entry
entry =
    Decode.oneOf [ fileEntry, directoryEntry ]


fileEntry : Decode.Decoder Entry
fileEntry =
    decode File
        |> required "location" Decode.string


directoryEntry : Decode.Decoder Entry
directoryEntry =
    decode Directory
        |> custom (Decode.list (Decode.lazy (\_ -> entry)))
