module ImageDirectory exposing (..)

import Html
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom)


main =
    let
        text =
            case example of
                Ok json ->
                    let
                        value =
                            encode json
                    in

                    Encode.encode 4 value

                Err error -> error
    in
        Html.text text


example : Result String Entry
example =
    Decode.decodeString directoryEntry """{
  "type": "directory",
  "contents": [
    { "type": "file", "location":"a"},
    { "type": "file", "location":"b"},
    { "type": "directory", "contents": [{ "type": "file", "location":"c"}] }
  ]
}"""

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
                [ ( "type", Encode.string "file" )
                , ( "location", Encode.string location )
                ]

        Directory entries ->
            let
                contents =
                    entries
                        |> List.map encode
                        |> Encode.list
            in
                Encode.object
                    [ ( "type", Encode.string "directory" )
                    , ( "contents", contents )
                    ]



-- Decode


entry : Decode.Decoder Entry
entry =
    Decode.field "type" Decode.string
        |> Decode.andThen selectDecoder

selectDecoder : String -> Decode.Decoder Entry
selectDecoder type_ =
    case type_ of
        "file" -> fileEntry

        "directory" -> directoryEntry

        _ -> Decode.fail ("Unknown Entry type: \"" ++ type_ ++ "\"")


fileEntry : Decode.Decoder Entry
fileEntry =
    decode File
        |> required "location" Decode.string


directoryEntry : Decode.Decoder Entry
directoryEntry =
    decode Directory
        |> required "contents" (Decode.list (Decode.lazy (\_ -> entry)))
