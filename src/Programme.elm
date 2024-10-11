module Programme exposing (Output, Programme, decode, encode)

import Deck exposing (Deck)
import Json.Decode
import Json.Encode
import Style exposing (Style)


type alias Programme =
    { decks : List Deck
    , outputs : List Output
    }


type alias Output =
    { name : String
    , style : Style
    }


encode : Programme -> Json.Encode.Value
encode { decks, outputs } =
    Json.Encode.object
        [ ( "decks", Json.Encode.list Deck.encode decks )
        , ( "outputs", Json.Encode.list encodeOutput outputs )
        ]


decode : Json.Decode.Decoder Programme
decode =
    Json.Decode.map2 Programme
        (Json.Decode.field "decks" (Json.Decode.list Deck.decode))
        (Json.Decode.field "outputs" (Json.Decode.list decodeOutput))


encodeOutput : Output -> Json.Decode.Value
encodeOutput { name, style } =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "style", Style.encode style )
        ]


decodeOutput : Json.Decode.Decoder Output
decodeOutput =
    Json.Decode.map2 Output
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "style" Style.decode)
