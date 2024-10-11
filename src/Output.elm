port module Output exposing (LiveModel, Model, Msg(..), init, liveSubscriptions, main, send, subscriptions, update, view)

import Animator
import Animator.Css
import Basics.Extra exposing (flip)
import Browser
import Browser.Navigation
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Json.Decode
import Json.Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Slide exposing (Slide)
import Style exposing (Size, Style)
import Time
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query


port liveRecv : (Json.Decode.Value -> msg) -> Sub msg


port liveSend : Json.Encode.Value -> Cmd msg


type alias Model a =
    { a
        | slide : Maybe Slide
        , boxAttributes : Animator.Timeline (Maybe BoxAttributes)
    }


type Msg
    = ShowSlide (Maybe Slide)
    | Tick Time.Posix
    | NOP


type alias LiveModel =
    { title : String
    , slide : Int
    , slides : List ( String, Dict String Slide )
    }


main : Program {} (Model { output : String }) Msg
main =
    Browser.application
        { init = init_
        , view = \model -> { title = "Breaking Cues " ++ model.output, body = [ view model ] }
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \({ output } as model) ->
                Sub.batch
                    [ subscriptions model
                    , liveSubscriptions
                        (ShowSlide
                            << Maybe.andThen
                                (\{ slide, slides } ->
                                    Maybe.andThen (Dict.get output << Tuple.second) <|
                                        List.getAt slide slides
                                )
                        )
                    ]
        , onUrlRequest = \_ -> NOP
        , onUrlChange = \_ -> NOP
        }


init_ : {} -> Url -> Browser.Navigation.Key -> ( Model { output : String }, Cmd msg )
init_ {} url _ =
    ( { output = Maybe.withDefault "" <| Maybe.join <| Url.Parser.parse (Url.Parser.query <| Url.Parser.Query.string "output") { url | path = "" }
      , slide = Nothing
      , boxAttributes = Animator.init Nothing
      }
    , Cmd.none
    )


init : Maybe Slide -> Model {}
init slide =
    { slide = slide
    , boxAttributes = Animator.init <| Maybe.map boxAttributesFromSlide slide
    }


view : Model a -> Html msg
view model =
    case model.boxAttributes |> Maybe.oneOf [ Animator.current, Animator.previous ] of
        Just boxAttributes ->
            box model.boxAttributes boxAttributes <|
                case model.slide of
                    Just ({ style, contents } as slide) ->
                        case contents of
                            Slide.Title { title, subtitle } ->
                                [ Html.p
                                    [ Html.Attributes.style "margin" "0px"
                                    , fontSize style.titleSize
                                    ]
                                    [ textWithTags title ]
                                , Html.p
                                    [ Html.Attributes.style "margin" "0px"
                                    , fontSize style.subtitleSize
                                    , Html.Attributes.style "position" "absolute"
                                    , Html.Attributes.style "top" <| String.fromFloat (boxAttributes.paddingV + boxAttributes.lineHeight) ++ "px"
                                    , Html.Attributes.style "line-height" "1"
                                    ]
                                    [ textWithTags subtitle ]
                                ]

                            Slide.Body { lines } ->
                                [ Html.div [ fontSize style.bodySize ] [ textWithTags <| String.replace "~" "" <| String.concat <| List.map (\line -> line ++ "\n") lines ] ]

                    Nothing ->
                        []

        Nothing ->
            Html.div [] []


textWithTags : String -> Html msg
textWithTags text =
    case Html.Parser.run Html.Parser.allCharRefs text of
        Ok nodes ->
            Html.div [] <| Html.Parser.nodesToHtml nodes

        Err _ ->
            Html.text text


type alias BoxAttributes =
    { size : Size
    , backgroundColour : String
    , textColour : String
    , fontFamily : String
    , lineHeight : Float
    , cornerRadius : Float
    , paddingH : Float
    , paddingV : Float
    , left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


boxAttributesFromSlide : Slide -> BoxAttributes
boxAttributesFromSlide { style, contents, lineHeight } =
    let
        scale : Float -> Float
        scale =
            (*) (toFloat style.size.height)

        margin : Float
        margin =
            scale style.margin

        paddingH : Float
        paddingH =
            scale <|
                case contents of
                    Slide.Title {} ->
                        style.titlePaddingH

                    Slide.Body {} ->
                        style.bodyPaddingH

        paddingV : Float
        paddingV =
            scale <|
                case contents of
                    Slide.Title {} ->
                        style.titlePaddingV

                    Slide.Body {} ->
                        style.bodyPaddingV

        numLines : Int
        numLines =
            case contents of
                Slide.Title {} ->
                    1

                Slide.Body { lines } ->
                    List.length lines

        width : Float
        width =
            case style.cases of
                Style.Full ->
                    toFloat style.size.width

                Style.Horizontal {} ->
                    toFloat style.size.width - 2 * margin

                Style.Vertical { split } ->
                    split * (toFloat style.size.width - 2 * margin)

        height : Float
        height =
            case style.cases of
                Style.Full ->
                    toFloat style.size.height

                Style.Horizontal {} ->
                    lineHeight * toFloat numLines + 2 * paddingV

                Style.Vertical {} ->
                    toFloat style.size.height - 2 * margin
    in
    { size = style.size
    , backgroundColour = style.backgroundColour
    , textColour = style.textColour
    , fontFamily = style.fontFamily
    , lineHeight = lineHeight
    , cornerRadius =
        min
            (scale <|
                case contents of
                    Slide.Title {} ->
                        style.titleCornerRadius

                    Slide.Body {} ->
                        style.bodyCornerRadius
            )
            (min width height / 2)
    , paddingH = paddingH
    , paddingV = paddingV
    , left =
        case style.cases of
            Style.Full ->
                0

            Style.Horizontal { align } ->
                margin

            Style.Vertical { align, split } ->
                case align of
                    Style.Left ->
                        margin

                    Style.HCenter ->
                        (toFloat style.size.width - width) / 2

                    Style.Right ->
                        toFloat style.size.width - width - margin
    , right =
        case style.cases of
            Style.Full ->
                0

            Style.Horizontal { align } ->
                margin

            Style.Vertical { align, split } ->
                case align of
                    Style.Left ->
                        toFloat style.size.width - width - margin

                    Style.HCenter ->
                        (toFloat style.size.width - width) / 2

                    Style.Right ->
                        margin
    , top =
        case style.cases of
            Style.Full ->
                0

            Style.Horizontal { align } ->
                case align of
                    Style.Top ->
                        margin

                    Style.VCenter ->
                        (toFloat style.size.height - height) / 2

                    Style.Bottom ->
                        toFloat style.size.height - margin - height

            Style.Vertical { align, split } ->
                margin
    , bottom =
        case style.cases of
            Style.Full ->
                0

            Style.Horizontal { align } ->
                case align of
                    Style.Top ->
                        toFloat style.size.height - margin - height

                    Style.VCenter ->
                        (toFloat style.size.height - height) / 2

                    Style.Bottom ->
                        margin

            Style.Vertical { align, split } ->
                margin
    }


box : Animator.Timeline (Maybe BoxAttributes) -> BoxAttributes -> List (Html msg) -> Html msg
box boxAttributes default =
    let
        centerX =
            (default.left + toFloat default.size.width - default.right) / 2

        centerY =
            (default.top + toFloat default.size.height - default.bottom) / 2
    in
    Animator.Css.div boxAttributes
        [ Animator.Css.opacity <|
            \boxAttributes2 ->
                Animator.at <|
                    case boxAttributes2 of
                        Nothing ->
                            0

                        Just _ ->
                            1
        , Animator.Css.backgroundColor <| Maybe.withDefault clear << Maybe.andThen (parseColour << .backgroundColour)
        , Animator.Css.fontColor <| Maybe.withDefault Color.black << Maybe.andThen (parseColour << .textColour)
        , pixelAttribute "border-radius" <| Maybe.unwrap 0 .cornerRadius
        , pixelAttribute "padding-left" <| Maybe.unwrap 0 .paddingH
        , pixelAttribute "padding-right" <| Maybe.unwrap 0 .paddingH
        , pixelAttribute "padding-top" <| Maybe.unwrap 0 .paddingV
        , pixelAttribute "padding-bottom" <| Maybe.unwrap 0 .paddingV
        , pixelAttribute "left" <| Maybe.unwrap centerX .left
        , pixelAttribute "right" <| Maybe.unwrap (toFloat default.size.width - centerX) .right
        , pixelAttribute "top" <| Maybe.unwrap centerY .top
        , pixelAttribute "bottom" <| Maybe.unwrap (toFloat default.size.height - centerY) .bottom
        , pixelAttribute "line-height" <| Maybe.unwrap 0 .lineHeight
        ]
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "font-family" default.fontFamily
        , Html.Attributes.style "white-space" "pre-wrap"
        ]


clear : Color
clear =
    Color.fromRgba { red = 0, green = 0, blue = 0, alpha = 0 }


parseColour : String -> Maybe Color
parseColour =
    let
        hexDigitToInt : String -> Maybe Int
        hexDigitToInt =
            flip List.elemIndex [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F" ] << String.toUpper

        hexDigit : Parser Int
        hexDigit =
            Parser.chompIf Char.isHexDigit
                |> Parser.getChompedString
                |> Parser.andThen (Maybe.unwrap (Parser.problem "Invalid hex digit") Parser.succeed << hexDigitToInt)

        byte : Parser Int
        byte =
            Parser.succeed (+)
                |= Parser.map ((*) 0x10) hexDigit
                |= hexDigit

        component : Parser Float
        component =
            Parser.map (\x -> toFloat x / 0xFF) byte
    in
    Result.toMaybe
        << Parser.run
            (Parser.succeed Color.rgba
                |. Parser.symbol "#"
                |= component
                |= component
                |= component
                |= Parser.oneOf [ component, Parser.succeed 1 ]
                |. Parser.end
            )


pixelAttribute : String -> (state -> Float) -> Animator.Css.Attribute state
pixelAttribute name getValue =
    Animator.Css.style name (\value -> String.fromFloat value ++ "px") (Animator.at << getValue)


fontSize : Float -> Html.Attribute msg
fontSize size =
    Html.Attributes.style "font-size" (String.fromFloat size ++ "pt")


animator : Animator.Animator (Model a)
animator =
    Animator.animator
        |> Animator.watchingWith .boxAttributes
            (\boxAttributes model ->
                { model | boxAttributes = boxAttributes }
            )
            (\_ -> False)


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        ShowSlide slide ->
            { model
                | slide = slide
                , boxAttributes = goFromHere Animator.quickly (Maybe.map boxAttributesFromSlide slide) model.boxAttributes
            }

        Tick time ->
            Animator.update time animator model

        NOP ->
            model


goFromHere : Animator.Duration -> state -> Animator.Timeline state -> Animator.Timeline state
goFromHere duration ev timeline =
    Animator.interrupt
        [ Animator.event
            (if Animator.current timeline == Animator.arrived timeline then
                Animator.immediately

             else
                duration
            )
            (Animator.current timeline)
        , Animator.event duration ev
        ]
        timeline


subscriptions : Model a -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator


liveSubscriptions : (Maybe LiveModel -> msg) -> Sub msg
liveSubscriptions msg =
    liveRecv <|
        Result.withDefault (msg Nothing)
            << Json.Decode.decodeValue
                (Json.Decode.map msg <|
                    Json.Decode.maybe <|
                        Json.Decode.map3 LiveModel
                            (Json.Decode.field "title" Json.Decode.string)
                            (Json.Decode.field "slide" Json.Decode.int)
                            (Json.Decode.field "slides" <|
                                Json.Decode.list <|
                                    Json.Decode.map2 Tuple.pair
                                        (Json.Decode.field "operator" Json.Decode.string)
                                        (Json.Decode.field "slides" <| Json.Decode.dict Slide.decode)
                            )
                )


send : Maybe LiveModel -> Cmd msg
send model =
    case model of
        Nothing ->
            liveSend Json.Encode.null

        Just { title, slide, slides } ->
            liveSend <|
                Json.Encode.object
                    [ ( "title", Json.Encode.string title )
                    , ( "slide", Json.Encode.int slide )
                    , ( "slides"
                      , Json.Encode.list
                            (\( operator, slides_ ) ->
                                Json.Encode.object
                                    [ ( "operator", Json.Encode.string operator )
                                    , ( "slides", Json.Encode.dict identity Slide.encode slides_ )
                                    ]
                            )
                            slides
                      )
                    ]
