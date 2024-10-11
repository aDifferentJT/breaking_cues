module Fetch.Psalm exposing (Bold(..), Params, Psalter(..), default, fetch, parseBCPPage)

import Deck
import Fetch.Common exposing (..)
import Html.Parser
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=))
import Task exposing (Task)
import Url.Builder


type Psalter
    = BCP
    | CW


type Bold
    = None
    | EvenVerses
    | SecondHalf


type alias Params =
    { psalter : Psalter
    , number : Int
    , startVerse : Int
    , endVerse : Int
    , gloria : Bool
    , bold : Bold
    }


default : Params
default =
    { psalter = BCP
    , number = 1
    , startVerse = 1
    , endVerse = 176
    , gloria = False
    , bold = None
    }


fetch : Params -> Task String (List Deck.Chunk)
fetch params =
    case params.psalter of
        BCP ->
            fetchBCP params

        CW ->
            fetchCW params


fetchBCP : Params -> Task String (List Deck.Chunk)
fetchBCP params =
    get
        (Url.Builder.crossOrigin
            "https://www.churchofengland.org"
            [ "prayer-and-worship", "worship-texts-and-resources", "book-common-prayer", "psalter" ]
            []
        )
        |> Task.andThen
            (\contentsDocument ->
                contentsDocument.root
                    |> getElementsByClassName "prayerContainer"
                    |> List.concatMap getChildren
                    |> List.filter (not << elementHasClassName "vlcopyright")
                    |> List.concatMap (getElementsByTagName "a")
                    |> List.filterMap
                        (\element ->
                            Maybe.map2 (\page url -> ( page, url ))
                                (parseBCPPage <| getText element)
                                (elementGetAttribute "href" element
                                    |> Maybe.map
                                        (\urlString ->
                                            Url.Builder.crossOrigin
                                                "https://www.churchofengland.org"
                                                [ urlString ]
                                                []
                                        )
                                )
                        )
                    |> List.filter (\( page, url ) -> pageOverlapsWithParams page params)
                    |> List.map
                        (\( page, url ) ->
                            get url
                                |> Task.andThen
                                    (\versesDocument ->
                                        versesDocument.root
                                            |> getElementsByClassName "prayerContainer"
                                            |> List.concatMap getChildren
                                            |> (if params.number == 119 then
                                                    Just

                                                else
                                                    List.groupWhile
                                                        (\_ element -> not <| elementHasClassName "vlitemheading" element)
                                                        >> List.tail
                                                        >> Maybe.andThen
                                                            (List.find
                                                                (\( element, _ ) ->
                                                                    getText element == "Psalm " ++ String.fromInt params.number ++ "."
                                                                )
                                                            )
                                                        >> Maybe.map List.cons
                                               )
                                            |> Maybe.map
                                                (List.filter (elementHasClassName "vlpsalm")
                                                    >> filterTreeBy (not << elementHasClassName "vlversenumber")
                                                    >> List.map
                                                        (getText
                                                            >> String.trim
                                                            >> String.split ":"
                                                            >> List.indexedMap
                                                                (\index verse ->
                                                                    if params.bold == SecondHalf && modBy 2 index == 1 then
                                                                        "<b>" ++ verse ++ "</b>"

                                                                    else
                                                                        verse
                                                                )
                                                            >> String.join ":"
                                                        )
                                                    >> selectVersesFromPage page params
                                                )
                                            |> Maybe.unwrap (Task.fail "Html not as expected") Task.succeed
                                    )
                        )
                    |> Task.sequence
                    |> Task.map
                        (List.concat
                            >> List.indexedMap
                                (\index verse ->
                                    if params.bold == EvenVerses && modBy 2 index == 1 then
                                        "<b>" ++ verse ++ "</b>"

                                    else
                                        verse
                                )
                            >> String.join "\n"
                            >> (\verses ->
                                    List.concat
                                        [ [ Deck.Title
                                                { title = getTitle params
                                                , subtitle = "Book of Common Prayer, Crown Copyright"
                                                }
                                          , Deck.Body verses
                                          ]
                                        , if params.gloria then
                                            [ Deck.Body <|
                                                let
                                                    gloria =
                                                        """Glory be to the Father, and to the Son,
and to the Holy Ghost;
As it was in the beginning, is now,
and ever shall be, world without end. Amen."""
                                                in
                                                if params.bold == None then
                                                    gloria

                                                else
                                                    "<b>" ++ gloria ++ "</b>"
                                            ]

                                          else
                                            []
                                        ]
                               )
                        )
            )


fetchCW : Params -> Task String (List Deck.Chunk)
fetchCW params =
    get
        (Url.Builder.crossOrigin
            "https://www.churchofengland.org"
            [ "prayer-and-worship", "worship-texts-and-resources", "common-worship", "common-material", "psalter", "psalm-" ++ String.fromInt params.number ]
            []
        )
        |> Task.andThen
            (\{ root } ->
                root
                    |> getElementsByClassName "cw"
                    |> List.concatMap getChildren
                    |> List.filter (Maybe.withDefault False << Maybe.map (String.contains "ve") << elementGetAttribute "class")
                    |> List.groupWhile (\_ element -> not <| elementHasClassName "ve1" element)
                    |> List.map
                        (List.cons
                            >> List.groupWhile
                                (\_ element -> not <| elementHasClassName "vein" element && elementHasClassName "indent1" element)
                            >> List.map
                                (List.cons
                                    >> List.map (getText >> trimLeadingInteger >> String.trim)
                                    >> String.join "\n"
                                )
                            >> List.indexedMap
                                (\index verse ->
                                    if params.bold == SecondHalf && modBy 2 index == 1 then
                                        "<b>" ++ verse ++ "</b>"

                                    else
                                        verse
                                )
                            >> String.join "\n"
                        )
                    |> List.indexedMap
                        (\index verse ->
                            if params.bold == EvenVerses && modBy 2 index == 1 then
                                "<b>" ++ verse ++ "</b>"

                            else
                                verse
                        )
                    |> slice (params.startVerse - 1) params.endVerse
                    |> String.join "\n"
                    |> (\verses ->
                            List.concat
                                [ [ Deck.Title
                                        { title = getTitle params
                                        , subtitle = "Common Worship © The Archbishops' Council 2000"
                                        }
                                  , Deck.Body verses
                                  ]
                                , if params.gloria then
                                    [ Deck.Body <|
                                        let
                                            gloria =
                                                """Glory to the Father and to the Son
and to the Holy Spirit;
as it was in the beginning is now,
and shall be for ever. Amen."""
                                        in
                                        if params.bold == None then
                                            gloria

                                        else
                                            "<b>" ++ gloria ++ "</b>"
                                    ]

                                  else
                                    []
                                ]
                       )
                    |> Task.succeed
            )


getTitle : Params -> String
getTitle { number, startVerse, endVerse } =
    if startVerse == default.startVerse && endVerse == default.endVerse then
        "Psalm " ++ String.fromInt number

    else if endVerse == default.endVerse then
        "Psalm " ++ String.fromInt number ++ ": " ++ String.fromInt startVerse ++ "-end"

    else
        "Psalm " ++ String.fromInt number ++ ": " ++ String.fromInt startVerse ++ "-" ++ String.fromInt endVerse


type BCPPage
    = Not119 { start : Int, end : Int }
    | Is119 { startVerse : Int, endVerse : Int }


parseBCPPage : String -> Maybe BCPPage
parseBCPPage =
    let
        int =
            Parser.andThen (Maybe.unwrap (Parser.problem "Invalid int") Parser.succeed << String.toInt) <| Parser.getChompedString <| Parser.chompWhile Char.isDigit

        terminator =
            Parser.chompIf (\c -> c == ':' || c == '.')
    in
    Result.toMaybe
        << Parser.run
            (Parser.oneOf
                [ Parser.succeed (\startVerse endVerse -> Is119 { startVerse = startVerse, endVerse = endVerse })
                    |. Parser.token "Psalm 119 "
                    |= int
                    |. Parser.token "-"
                    |= int
                    |. terminator
                , Parser.succeed (\start end -> Not119 { start = start, end = end })
                    |. Parser.token "Psalms "
                    |= int
                    |. Parser.token "-"
                    |= int
                    |. terminator
                , Parser.succeed (\number -> Not119 { start = number, end = number })
                    |. Parser.token "Psalm "
                    |= int
                    |. terminator
                ]
            )


pageOverlapsWithParams : BCPPage -> Params -> Bool
pageOverlapsWithParams page ({ number } as params) =
    case page of
        Not119 { start, end } ->
            start <= number && number <= end

        Is119 page_ ->
            number == 119 && params.startVerse <= page_.endVerse && page_.startVerse <= params.endVerse


selectVersesFromPage : BCPPage -> Params -> List a -> List a
selectVersesFromPage page params =
    case page of
        Not119 {} ->
            slice (params.startVerse - 1) params.endVerse

        Is119 page_ ->
            slice
                (max params.startVerse page_.startVerse - page_.startVerse)
                (min params.endVerse page_.endVerse - page_.startVerse)


slice : Int -> Int -> List a -> List a
slice start end =
    List.take (end - start) << List.drop start
