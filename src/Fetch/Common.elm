module Fetch.Common exposing (..)

import Html.Parser
import Http
import Http.Tasks
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Task exposing (Task)
import Url.Builder


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


getText : Html.Parser.Node -> String
getText node =
    case node of
        Html.Parser.Text text ->
            text

        Html.Parser.Element _ _ children ->
            String.join " " <| List.map getText children

        Html.Parser.Comment _ ->
            ""


getChildren : Html.Parser.Node -> List Html.Parser.Node
getChildren node =
    case node of
        Html.Parser.Text _ ->
            []

        Html.Parser.Element _ _ children ->
            children

        Html.Parser.Comment _ ->
            []


getElementsBy : (Html.Parser.Node -> Bool) -> Html.Parser.Node -> List Html.Parser.Node
getElementsBy f node =
    let
        rest =
            List.concatMap (getElementsBy f) <| getChildren node
    in
    if f node then
        node :: rest

    else
        rest


filterTreeBy : (Html.Parser.Node -> Bool) -> List Html.Parser.Node -> List Html.Parser.Node
filterTreeBy f =
    List.filterMap <|
        \node ->
            if f node then
                case node of
                    Html.Parser.Text _ ->
                        Just node

                    Html.Parser.Element tag attributes children ->
                        Just <| Html.Parser.Element tag attributes <| filterTreeBy f children

                    Html.Parser.Comment _ ->
                        Just node

            else
                Nothing


getElementById : String -> Html.Parser.Node -> Maybe Html.Parser.Node
getElementById id =
    List.head << getElementsBy ((==) (Just id) << elementGetAttribute "id")


getElementsByTagName : String -> Html.Parser.Node -> List Html.Parser.Node
getElementsByTagName tag =
    getElementsBy
        (\node ->
            case node of
                Html.Parser.Text _ ->
                    False

                Html.Parser.Element tag_ _ _ ->
                    tag == tag_

                Html.Parser.Comment _ ->
                    False
        )


getElementsByClassName : String -> Html.Parser.Node -> List Html.Parser.Node
getElementsByClassName =
    getElementsBy << elementHasClassName


elementHasClassName : String -> Html.Parser.Node -> Bool
elementHasClassName className node =
    node
        |> elementGetAttribute "class"
        |> Maybe.map
            (String.words
                >> List.member className
            )
        |> Maybe.withDefault False


elementGetAttribute : String -> Html.Parser.Node -> Maybe String
elementGetAttribute attrName node =
    case node of
        Html.Parser.Text _ ->
            Nothing

        Html.Parser.Element _ attributes _ ->
            attributes
                |> List.find (\( attribute, _ ) -> attribute == attrName)
                |> Maybe.map (\( _, value ) -> value)

        Html.Parser.Comment _ ->
            Nothing


trimLeadingInteger : String -> String
trimLeadingInteger =
    Parser.run
        (Parser.oneOf
            [ Parser.succeed identity
                |. Parser.int
                |. Parser.spaces
                |= Parser.getChompedString (Parser.chompWhile (\_ -> True))
                |. Parser.end
            , Parser.succeed identity
                |= Parser.getChompedString (Parser.chompWhile (\_ -> True))
                |. Parser.end
            ]
        )
        >> Result.withDefault ""


get : String -> Task String Html.Parser.Document
get url =
    Http.Tasks.get
        { url =
            Url.Builder.crossOrigin
                "http://127.0.0.1:3000"
                [ "proxy" ]
                [ Url.Builder.string "url" url ]
        , resolver = Http.Tasks.resolveString
        }
        |> Task.mapError errorToString
        |> Task.andThen
            (\body ->
                case Html.Parser.runDocument Html.Parser.allCharRefs body of
                    Err deadEnds ->
                        Task.fail <| "Invalid HTML:\n" ++ body

                    Ok document ->
                        Task.succeed document
            )
