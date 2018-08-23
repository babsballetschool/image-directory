module ImageDirectory exposing (Entry, encode, decoder, view)

{-| Elm project showing an image directory structure.

@docs Entry, encode, decoder, view

-}

import Browser exposing (element)
import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Decode as Decode exposing (succeed)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode


main : Program () Entry Message
main =
    let
        entry =
            case example of
                Ok example_entry ->
                    example_entry

                Err error ->
                    File "something went wrong"
    in
    element
        { init = \_ -> ( entry, Cmd.none )
        , view = view Clicked
        , update = update
        , subscriptions = \_ -> Sub.none
        }


example : Result Decode.Error Entry
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


{-| Represents the directory structure for an image directory.
-}
type Entry
    = File String
    | Directory (List Entry)



-- Encode


{-| Encodes an Entry into a `Json.Encode.Value`
-}
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
                        |> Encode.list encode
            in
            Encode.object
                [ ( "type", Encode.string "directory" )
                , ( "contents", contents )
                ]



-- Decode


{-| Decodes an Json representation of an `Entry`
-}
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
    succeed File
        |> required "location" Decode.string


directoryEntry : Decode.Decoder Entry
directoryEntry =
    succeed Directory
        |> required "contents" (Decode.list (Decode.lazy (\_ -> decoder)))



-- Update


type Message
    = Clicked String


update : msg -> Entry -> ( Entry, Cmd msg )
update message entry =
    ( entry, Cmd.none )



-- View


{-| Accepts a `onFileClick` function and returns a view for an Entry
-}
view : (String -> msg) -> Entry -> Html.Html msg
view onFileClick entry =
    case entry of
        File location ->
            fileView (onFileClick location) location

        Directory entries ->
            directoryView onFileClick entries


fileView : msg -> String -> Html.Html msg
fileView message location =
    Html.div [ Attribute.class "file", Event.onClick message ]
        [ Html.img [ Attribute.src location ] []
        , Html.span [] [ Html.text location ]
        ]


directoryView : (String -> msg) -> List Entry -> Html.Html msg
directoryView onFileClick entries =
    let
        contents =
            List.map (view onFileClick) entries
    in
    Html.div [ Attribute.class "directory" ] contents
