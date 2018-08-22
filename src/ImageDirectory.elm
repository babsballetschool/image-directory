module ImageDirectory exposing (Entry, encode, decoder, view)

import Html exposing (program)
import Html.Attributes as Attribute
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom)


main : Program Never Entry msg
main =
    let
        entry =
            case example of
                Ok entry ->
                    entry

                Err error ->
                    File "something went wrong"
    in
        program
            { init = ( entry, Cmd.none )
            , view = view
            , update = update
            , subscriptions = \_ -> Sub.none
            }


example : Result String Entry
example =
    Decode.decodeString directoryEntry """{
  "type": "directory",
  "contents": [
    { "type": "file", "location":"http://via.placeholder.com/20x20"},
    { "type": "file", "location":"http://via.placeholder.com/20x30"},
    { "type": "directory", "contents": [{ "type": "file", "location":"http://via.placeholder.com/30x20"}] }
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


decoder : Decode.Decoder Entry
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen selectDecoder


selectDecoder : String -> Decode.Decoder Entry
selectDecoder type_ =
    case type_ of
        "file" ->
            fileEntry

        "directory" ->
            directoryEntry

        _ ->
            Decode.fail ("Unknown Entry type: \"" ++ type_ ++ "\"")


fileEntry : Decode.Decoder Entry
fileEntry =
    decode File
        |> required "location" Decode.string


directoryEntry : Decode.Decoder Entry
directoryEntry =
    decode Directory
        |> required "contents" (Decode.list (Decode.lazy (\_ -> decoder)))


-- Update

update : msg -> Entry -> (Entry, Cmd msg)
update _ entry =
    (entry, Cmd.none)

-- View


view : Entry -> Html.Html msg
view entry =
    case entry of
        File location ->
            fileView location

        Directory entries ->
            directoryView entries


fileView : String -> Html.Html msg
fileView location =
    Html.div [ Attribute.class "file" ]
        [ Html.img [ Attribute.src location ] []
        , Html.span [] [ Html.text location ]
        ]


directoryView : List Entry -> Html.Html msg
directoryView entries =
    let
        contents =
            List.map view entries
    in
        Html.div [ Attribute.class "directory" ] contents
