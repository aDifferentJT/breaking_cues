module Fetch.Hymn exposing (Hymnal(..), Params, default, fetch)

import Deck
import Fetch.Common exposing (..)
import Html.Parser
import List.Extra as List
import Parser
import Task exposing (Task)
import Url.Builder


type Hymnal
    = NEH
    | AM


type alias Params =
    { hymnal : Hymnal
    , number : Int
    }


default : Params
default =
    { hymnal = NEH
    , number = 1
    }


fetch : Params -> Task String (List Deck.Chunk)
fetch { hymnal, number } =
    get
        (Url.Builder.crossOrigin
            "https://hymnary.org"
            [ "hymn", hymnalURLComponent hymnal, String.fromInt number ]
            []
        )
        |> Task.onError
            (\_ ->
                get
                    (Url.Builder.crossOrigin
                        "https://hymnary.org"
                        [ "hymn", hymnalURLComponent hymnal, String.fromInt number ++ "a" ]
                        []
                    )
            )
        |> Task.andThen
            (\{ root } ->
                let
                    title =
                        root
                            |> getElementsByClassName "hymntitle"
                            |> List.head
                            |> Maybe.map getText
                            |> Maybe.withDefault ("Hymn " ++ String.fromInt number)

                    author =
                        root
                            |> getElementsByClassName "result-row"
                            |> List.find (String.contains "Author" << getText)
                            |> Maybe.andThen
                                (getElementsByClassName "hy_infoItem"
                                    >> List.head
                                )
                            |> Maybe.map getText
                            |> Maybe.withDefault ""

                    verses =
                        root
                            |> getElementById "text"
                            |> Maybe.map
                                (getChildren
                                    >> List.map (getText >> trimLeadingInteger >> String.split "\n" >> List.map String.trim >> String.join "\n")
                                    >> List.filter ((/=) "")
                                )
                            |> Maybe.withDefault []
                in
                Task.succeed
                    (Deck.Title
                        { title = title
                        , subtitle = author
                        }
                        :: List.map Deck.Body verses
                    )
            )


hymnalURLComponent : Hymnal -> String
hymnalURLComponent hymnal =
    case hymnal of
        NEH ->
            "NEH1985"

        AM ->
            "AM2013"
