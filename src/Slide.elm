module Slide exposing (Contents(..), Slide, collate, decode, encode)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import List.Extra as List
import Style exposing (Style)


type alias Slide =
    { style : Style
    , contents : Contents
    , lineHeight : Float
    }


type Contents
    = Title TitleSlide
    | Body BodySlide


type alias TitleSlide =
    { title : String
    , subtitle : String
    }


type alias BodySlide =
    { lines : List String
    }


toString : Slide -> String
toString { contents } =
    case contents of
        Title { title, subtitle } ->
            title ++ "\n" ++ subtitle

        Body { lines } ->
            String.join "\n" lines


collate : Dict String (List Slide) -> List ( String, Dict String Slide )
collate =
    collate2 << Dict.map (\_ -> List.map (\slide -> ( String.trim <| toString slide, slide )))


collate2 : Dict String (List ( String, Slide )) -> List ( String, Dict String Slide )
collate2 slides =
    let
        prefix : String
        prefix =
            slides
                |> Dict.values
                |> List.map
                    (\slides_ ->
                        case List.head slides_ of
                            Nothing ->
                                ""

                            Just ( text, slide ) ->
                                text
                    )
                |> List.minimumBy String.length
                |> Maybe.withDefault ""

        prefixLength : Int
        prefixLength =
            String.length prefix

        currentSlides : Dict String Slide
        currentSlides =
            slides
                |> Dict.map
                    (\_ slides_ ->
                        case List.head slides_ of
                            Nothing ->
                                { style = Style.default
                                , contents = Body { lines = [] }
                                , lineHeight = 0
                                }

                            Just ( _, slide ) ->
                                slide
                    )

        nextSlides : Dict String (List ( String, Slide ))
        nextSlides =
            slides
                |> Dict.map
                    (\_ slides2 ->
                        case slides2 of
                            [] ->
                                []

                            ( text, slide ) :: slides3 ->
                                let
                                    text2 =
                                        String.trim <| String.dropLeft prefixLength text
                                in
                                if String.isEmpty text2 then
                                    slides3

                                else
                                    ( text2, slide ) :: slides3
                    )

        done : Bool
        done =
            nextSlides |> Dict.values |> List.all List.isEmpty
    in
    ( prefix, currentSlides )
        :: (if done then
                []

            else
                collate2 nextSlides
           )


encode : Slide -> Json.Encode.Value
encode { style, contents, lineHeight } =
    Json.Encode.object <|
        List.concat
            [ [ ( "style", Style.encode style ) ]
            , case contents of
                Title { title, subtitle } ->
                    [ ( "type", Json.Encode.string "title" )
                    , ( "title", Json.Encode.string title )
                    , ( "subtitle", Json.Encode.string subtitle )
                    ]

                Body { lines } ->
                    [ ( "type", Json.Encode.string "body" )
                    , ( "lines", Json.Encode.list Json.Encode.string lines )
                    ]
            , [ ( "lineHeight", Json.Encode.float lineHeight ) ]
            ]


decode : Json.Decode.Decoder Slide
decode =
    Json.Decode.map3 Slide
        (Json.Decode.field "style" Style.decode)
        (Json.Decode.field "type" Json.Decode.string
            |> Json.Decode.andThen
                (\type_ ->
                    case type_ of
                        "title" ->
                            Json.Decode.map Title <|
                                Json.Decode.map2 TitleSlide
                                    (Json.Decode.field "title" Json.Decode.string)
                                    (Json.Decode.field "subtitle" Json.Decode.string)

                        "body" ->
                            Json.Decode.map Body <|
                                Json.Decode.map BodySlide
                                    (Json.Decode.field "lines" (Json.Decode.list Json.Decode.string))

                        _ ->
                            Json.Decode.fail "Invalid slide type"
                )
        )
        (Json.Decode.field "lineHeight" Json.Decode.float)
