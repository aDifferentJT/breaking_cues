module Fetch.Bible exposing (Framing(..), Params, default, fetch)

import Deck
import Fetch.Common exposing (..)
import Html.Parser
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Task exposing (Task)
import Url.Builder


type Framing
    = None
    | Standard
    | Gospel
    | LentGospel


type alias Params =
    { version : String
    , reference : String
    , framing : Framing
    }


default : Params
default =
    { version = "NRSVA"
    , reference = "Job 3:2"
    , framing = Standard
    }


fetch : Params -> Task String (List Deck.Chunk)
fetch { version, reference, framing } =
    get
        (Url.Builder.crossOrigin
            "https://www.biblegateway.com"
            [ "passage" ]
            [ Url.Builder.string "search" <| Regex.replace endRegex (\_ -> "10000") reference
            , Url.Builder.string "version" version
            , Url.Builder.string "interface" "print"
            ]
        )
        |> Task.andThen
            (\{ root } ->
                let
                    titleChunk : Deck.Chunk
                    titleChunk =
                        Deck.Title { title = reference, subtitle = version }

                    passageChunks : List Deck.Chunk
                    passageChunks =
                        root
                            |> getElementsByClassName "passage-content"
                            |> List.concatMap (getElementsByTagName "p")
                            |> List.map
                                (\element ->
                                    element
                                        |> getElementsByClassName "text"
                                        |> List.concatMap getChildren
                                        |> List.filter
                                            (\node ->
                                                case node of
                                                    Html.Parser.Text _ ->
                                                        True

                                                    Html.Parser.Element _ _ _ ->
                                                        not (elementHasClassName "chapternum" node)
                                                            && not (elementHasClassName "versenum" node)
                                                            && not (elementHasClassName "footnote" node)

                                                    Html.Parser.Comment _ ->
                                                        False
                                            )
                                        |> List.map getText
                                        |> String.join " "
                                        |> Regex.replace spaces (\_ -> " ")
                                        |> Deck.Body
                                )

                    gospelWriter : String
                    gospelWriter =
                        case
                            reference
                                |> String.toList
                                |> List.takeWhile Char.isAlpha
                                |> String.fromList
                        of
                            "" ->
                                "N"

                            writer ->
                                writer
                in
                Task.succeed <|
                    case framing of
                        None ->
                            List.concat
                                [ [ titleChunk ]
                                , passageChunks
                                ]

                        Standard ->
                            List.concat
                                [ [ titleChunk ]
                                , passageChunks
                                , [ Deck.Body "This is the word of the Lord.\n<b>Thanks be to God.</b>" ]
                                ]

                        Gospel ->
                            List.concat
                                [ [ Deck.Body "Alleluia, alleluia.\nAlleluia, alleluia."
                                  , Deck.Body "<b>Alleluia, alleluia.\nAlleluia, alleluia.</b>"
                                  , Deck.Title { title = "Gospel Acclamation", subtitle = "" }
                                  , Deck.Body "<b>Alleluia, alleluia.\nAlleluia, alleluia.</b>"
                                  , Deck.Body "The Lord be with you\n<b>and also with you.</b>"
                                  , Deck.Body <| "Hear the Gospel of our Lord Jesus Christ according to " ++ gospelWriter ++ "\n<b>Glory to you, O Lord.</b>"
                                  , titleChunk
                                  ]
                                , passageChunks
                                , [ Deck.Body "This is the Gospel of the Lord.\n<b>Praise to you, O Christ.</b>" ]
                                ]

                        LentGospel ->
                            List.concat
                                [ [ Deck.Body "Praise to you, O Christ,\nking of eternal glory!"
                                  , Deck.Body "<b>Praise to you, O Christ,\nking of eternal glory!</b>"
                                  , Deck.Title { title = "Gospel Acclamation", subtitle = "" }
                                  , Deck.Body "<b>Praise to you, O Christ,\nking of eternal glory!</b>"
                                  , Deck.Body "The Lord be with you\n<b>and also with you.</b>"
                                  , Deck.Body <| "Hear the Gospel of our Lord Jesus Christ according to " ++ gospelWriter ++ "\n<b>Glory to you, O Lord.</b>"
                                  , titleChunk
                                  ]
                                , passageChunks
                                , [ Deck.Body "This is the Gospel of the Lord.\n<b>Praise to you, O Christ.</b>" ]
                                ]
            )


spaces : Regex
spaces =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\s+"


endRegex : Regex
endRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = False, multiline = False } "end"
