module Deck exposing (Chunk(..), Deck, Variant, decode, decodeWithoutIdOrSlides, encode, encodeWithoutId, getTitle, new)

import Dict exposing (Dict)
import Json.Decode
import Json.Decode.Extra
import Json.Encode
import Slide exposing (Slide)
import Style exposing (LocalStyle)


type alias Deck =
    { title : Maybe String
    , variants : List Variant
    , id : Int
    }


type alias Variant =
    { name : String
    , chunks : List Chunk
    , slides : Dict String (List Slide)
    , style : Dict String LocalStyle
    }


type Chunk
    = Title TitleChunk
    | Body String


type alias TitleChunk =
    { title : String, subtitle : String }


getTitle : Deck -> String
getTitle deck =
    case deck.title of
        Just title ->
            title

        Nothing ->
            Maybe.withDefault "empty" <|
                case deck.variants of
                    [] ->
                        Nothing

                    { chunks } :: _ ->
                        case chunks of
                            (Title title) :: _ ->
                                if title.title == "" then
                                    Nothing

                                else
                                    Just title.title

                            (Body "") :: _ ->
                                Nothing

                            (Body body) :: _ ->
                                body
                                    |> String.lines
                                    |> List.head

                            [] ->
                                Nothing


new : Int -> Deck
new id =
    { id = id
    , title = Nothing
    , variants = [ { name = "", chunks = [ Body "New Deck" ], slides = Dict.empty, style = Dict.empty } ]
    }


encode : Deck -> Json.Encode.Value
encode { id, title, variants } =
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "title"
          , case title of
                Just title_ ->
                    Json.Encode.string title_

                Nothing ->
                    Json.Encode.null
          )
        , ( "variants", Json.Encode.list encodeVariant variants )

        -- Slides are not encoded as they shouldn't be sent serverward
        ]


encodeWithoutId : Deck -> Json.Encode.Value
encodeWithoutId { title, variants } =
    Json.Encode.object
        [ ( "title"
          , case title of
                Just title_ ->
                    Json.Encode.string title_

                Nothing ->
                    Json.Encode.null
          )
        , ( "variants", Json.Encode.list encodeVariant variants )

        -- Slides are not encoded as they shouldn't be sent serverward
        ]


decode : Json.Decode.Decoder Deck
decode =
    Json.Decode.map3 Deck
        (Json.Decode.field "title" <| Json.Decode.maybe Json.Decode.string)
        (Json.Decode.field "variants" <| Json.Decode.list decodeVariant)
        (Json.Decode.field "id" Json.Decode.int)


decodeWithoutIdOrSlides : Json.Decode.Decoder (Int -> Deck)
decodeWithoutIdOrSlides =
    Json.Decode.map2 Deck
        (Json.Decode.field "title" <| Json.Decode.maybe Json.Decode.string)
        (Json.Decode.field "variants" <| Json.Decode.list decodeVariantWithoutSlides)


encodeVariant : Variant -> Json.Encode.Value
encodeVariant { name, chunks, style } =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "chunks", Json.Encode.list encodeChunk chunks )
        , ( "style", Json.Encode.dict (\x -> x) Style.encodeLocal style )

        -- Slides are not encoded as they shouldn't be sent serverward
        ]


decodeVariant : Json.Decode.Decoder Variant
decodeVariant =
    Json.Decode.map4 Variant
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "chunks" (Json.Decode.list decodeChunk))
        (Json.Decode.field "slides" (Json.Decode.dict (Json.Decode.list Slide.decode)))
        (Json.Decode.field "style" (Json.Decode.dict Style.decodeLocal))


decodeVariantWithoutSlides : Json.Decode.Decoder Variant
decodeVariantWithoutSlides =
    Json.Decode.map4 Variant
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "chunks" (Json.Decode.list decodeChunk))
        (Json.Decode.succeed Dict.empty)
        (Json.Decode.field "style" (Json.Decode.dict Style.decodeLocal))


encodeChunk : Chunk -> Json.Encode.Value
encodeChunk chunk =
    case chunk of
        Title { title, subtitle } ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "title" )
                , ( "title", Json.Encode.string title )
                , ( "subtitle", Json.Encode.string subtitle )
                ]

        Body body ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "body" )
                , ( "body", Json.Encode.string body )
                ]


decodeChunk : Json.Decode.Decoder Chunk
decodeChunk =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "title" ->
                        Json.Decode.map Title <|
                            Json.Decode.map2 TitleChunk
                                (Json.Decode.field "title" Json.Decode.string)
                                (Json.Decode.field "subtitle" Json.Decode.string)

                    "body" ->
                        Json.Decode.map Body
                            (Json.Decode.field "body" Json.Decode.string)

                    _ ->
                        Json.Decode.fail "Invalid chunk type"
            )
