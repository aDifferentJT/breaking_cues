module Style exposing (ComparableStyle, HAlign(..), LocalStyle, Size, Style, StyleCases(..), VAlign(..), addLocal, decode, decodeLocal, default, emptyLocal, encode, encodeLocal, styleToComparable)

import Basics.Extra exposing (uncurry)
import Json.Decode
import Json.Decode.Extra
import Json.Encode


type alias Style =
    { size : Size
    , cases : StyleCases
    , margin : Float
    , titleCornerRadius : Float
    , bodyCornerRadius : Float
    , backgroundColour : String
    , textColour : String
    , titlePaddingH : Float
    , titlePaddingV : Float
    , bodyPaddingH : Float
    , bodyPaddingV : Float
    , maxLines : Int
    , fontFamily : String
    , titleSize : Float
    , subtitleSize : Float
    , bodySize : Float
    }


type alias ComparableStyle =
    ( ComparableSize
    , ( ComparableStyleCases
      , ( Float
        , ( Float
          , ( Float
            , ( String
              , ( String
                , ( Float
                  , ( Float
                    , ( Float
                      , ( Float
                        , ( Int
                          , ( String
                            , ( Float
                              , ( Float
                                , Float
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


styleToComparable : Style -> ComparableStyle
styleToComparable { size, cases, margin, titleCornerRadius, bodyCornerRadius, backgroundColour, textColour, titlePaddingH, titlePaddingV, bodyPaddingH, bodyPaddingV, maxLines, fontFamily, titleSize, subtitleSize, bodySize } =
    ( sizeToComparable size
    , ( styleCasesToComparable cases
      , ( margin
        , ( titleCornerRadius
          , ( bodyCornerRadius
            , ( backgroundColour
              , ( textColour
                , ( titlePaddingH
                  , ( titlePaddingV
                    , ( bodyPaddingH
                      , ( bodyPaddingV
                        , ( maxLines
                          , ( fontFamily
                            , ( titleSize
                              , ( subtitleSize
                                , bodySize
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


type alias Size =
    { height : Int
    , width : Int
    }


type alias ComparableSize =
    ( Int, Int )


sizeToComparable : Size -> ComparableSize
sizeToComparable { height, width } =
    ( height, width )


type StyleCases
    = Full
    | Horizontal
        { align : VAlign
        }
    | Vertical
        { align : HAlign
        , split : Float
        }


type alias ComparableStyleCases =
    ( Int, ComparableVAlign, ( ComparableHAlign, Float ) )


styleCasesToComparable : StyleCases -> ComparableStyleCases
styleCasesToComparable cases =
    case cases of
        Full ->
            ( 0, 0, ( 0, 0 ) )

        Horizontal { align } ->
            ( 1, vAlignToComparable align, ( 0, 0 ) )

        Vertical { align, split } ->
            ( 2, 0, ( hAlignToComparable align, split ) )


type HAlign
    = Left
    | HCenter
    | Right


type alias ComparableHAlign =
    Int


hAlignToComparable : HAlign -> ComparableHAlign
hAlignToComparable align =
    case align of
        Left ->
            0

        HCenter ->
            1

        Right ->
            2


type VAlign
    = Top
    | VCenter
    | Bottom


type alias ComparableVAlign =
    Int


vAlignToComparable : VAlign -> ComparableVAlign
vAlignToComparable align =
    case align of
        Top ->
            0

        VCenter ->
            1

        Bottom ->
            2


type alias LocalStyle =
    { cases : Maybe StyleCases
    , margin : Maybe Float
    , titleCornerRadius : Maybe Float
    , bodyCornerRadius : Maybe Float
    , backgroundColour : Maybe String
    , textColour : Maybe String
    , titlePaddingH : Maybe Float
    , titlePaddingV : Maybe Float
    , bodyPaddingH : Maybe Float
    , bodyPaddingV : Maybe Float
    , maxLines : Maybe Int
    , fontFamily : Maybe String
    , titleSize : Maybe Float
    , subtitleSize : Maybe Float
    , bodySize : Maybe Float
    }


addLocal : Style -> LocalStyle -> Style
addLocal style local =
    { size = style.size
    , cases = Maybe.withDefault style.cases local.cases
    , margin = Maybe.withDefault style.margin local.margin
    , titleCornerRadius = Maybe.withDefault style.titleCornerRadius local.titleCornerRadius
    , bodyCornerRadius = Maybe.withDefault style.bodyCornerRadius local.bodyCornerRadius
    , backgroundColour = Maybe.withDefault style.backgroundColour local.backgroundColour
    , textColour = Maybe.withDefault style.textColour local.textColour
    , titlePaddingH = Maybe.withDefault style.titlePaddingH local.titlePaddingH
    , titlePaddingV = Maybe.withDefault style.titlePaddingV local.titlePaddingV
    , bodyPaddingH = Maybe.withDefault style.bodyPaddingH local.bodyPaddingH
    , bodyPaddingV = Maybe.withDefault style.bodyPaddingV local.bodyPaddingV
    , maxLines = Maybe.withDefault style.maxLines local.maxLines
    , fontFamily = Maybe.withDefault style.fontFamily local.fontFamily
    , titleSize = Maybe.withDefault style.titleSize local.titleSize
    , subtitleSize = Maybe.withDefault style.subtitleSize local.subtitleSize
    , bodySize = Maybe.withDefault style.bodySize local.bodySize
    }


default : Style
default =
    { size = { width = 1920, height = 1080 }
    , cases = Horizontal { align = Bottom }
    , margin = 0.1
    , titleCornerRadius = 1
    , bodyCornerRadius = 0.01
    , backgroundColour = "#fff0c8b0"
    , textColour = "#000000"
    , titlePaddingH = 0.06
    , titlePaddingV = 0.03
    , bodyPaddingH = 0.02
    , bodyPaddingV = 0.02
    , maxLines = 3
    , fontFamily = "Times New Roman"
    , titleSize = 50
    , subtitleSize = 15
    , bodySize = 30
    }


emptyLocal : LocalStyle
emptyLocal =
    { cases = Nothing
    , margin = Nothing
    , titleCornerRadius = Nothing
    , bodyCornerRadius = Nothing
    , backgroundColour = Nothing
    , textColour = Nothing
    , titlePaddingH = Nothing
    , titlePaddingV = Nothing
    , bodyPaddingH = Nothing
    , bodyPaddingV = Nothing
    , maxLines = Nothing
    , fontFamily = Nothing
    , titleSize = Nothing
    , subtitleSize = Nothing
    , bodySize = Nothing
    }


encode : Style -> Json.Encode.Value
encode { size, cases, margin, titleCornerRadius, bodyCornerRadius, backgroundColour, textColour, titlePaddingH, titlePaddingV, bodyPaddingH, bodyPaddingV, maxLines, fontFamily, titleSize, subtitleSize, bodySize } =
    Json.Encode.object
        [ ( "size", encodeSize size )
        , ( "cases", encodeCases cases )
        , ( "margin", Json.Encode.float margin )
        , ( "titleCornerRadius", Json.Encode.float titleCornerRadius )
        , ( "bodyCornerRadius", Json.Encode.float bodyCornerRadius )
        , ( "backgroundColour", Json.Encode.string backgroundColour )
        , ( "textColour", Json.Encode.string textColour )
        , ( "titlePaddingH", Json.Encode.float titlePaddingH )
        , ( "titlePaddingV", Json.Encode.float titlePaddingV )
        , ( "bodyPaddingH", Json.Encode.float bodyPaddingH )
        , ( "bodyPaddingV", Json.Encode.float bodyPaddingV )
        , ( "maxLines", Json.Encode.int maxLines )
        , ( "fontFamily", Json.Encode.string fontFamily )
        , ( "titleSize", Json.Encode.float titleSize )
        , ( "subtitleSize", Json.Encode.float subtitleSize )
        , ( "bodySize", Json.Encode.float bodySize )
        ]


decode : Json.Decode.Decoder Style
decode =
    Json.Decode.succeed Style
        |> Json.Decode.Extra.andMap (Json.Decode.field "size" decodeSize)
        |> Json.Decode.Extra.andMap (Json.Decode.field "cases" decodeCases)
        |> Json.Decode.Extra.andMap (Json.Decode.field "margin" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "titleCornerRadius" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "bodyCornerRadius" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "backgroundColour" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "textColour" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "titlePaddingH" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "titlePaddingV" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "bodyPaddingH" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "bodyPaddingV" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "maxLines" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "fontFamily" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "titleSize" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "subtitleSize" Json.Decode.float)
        |> Json.Decode.Extra.andMap (Json.Decode.field "bodySize" Json.Decode.float)


encodeLocal : LocalStyle -> Json.Encode.Value
encodeLocal { cases, margin, titleCornerRadius, bodyCornerRadius, backgroundColour, textColour, titlePaddingH, titlePaddingV, bodyPaddingH, bodyPaddingV, maxLines, fontFamily, titleSize, subtitleSize, bodySize } =
    Json.Encode.object
        [ ( "cases", encodeMaybe encodeCases cases )
        , ( "margin", encodeMaybe Json.Encode.float margin )
        , ( "titleCornerRadius", encodeMaybe Json.Encode.float titleCornerRadius )
        , ( "bodyCornerRadius", encodeMaybe Json.Encode.float bodyCornerRadius )
        , ( "backgroundColour", encodeMaybe Json.Encode.string backgroundColour )
        , ( "textColour", encodeMaybe Json.Encode.string textColour )
        , ( "titlePaddingH", encodeMaybe Json.Encode.float titlePaddingH )
        , ( "titlePaddingV", encodeMaybe Json.Encode.float titlePaddingV )
        , ( "bodyPaddingH", encodeMaybe Json.Encode.float bodyPaddingH )
        , ( "bodyPaddingV", encodeMaybe Json.Encode.float bodyPaddingV )
        , ( "maxLines", encodeMaybe Json.Encode.int maxLines )
        , ( "fontFamily", encodeMaybe Json.Encode.string fontFamily )
        , ( "titleSize", encodeMaybe Json.Encode.float titleSize )
        , ( "subtitleSize", encodeMaybe Json.Encode.float subtitleSize )
        , ( "bodySize", encodeMaybe Json.Encode.float bodySize )
        ]


decodeLocal : Json.Decode.Decoder LocalStyle
decodeLocal =
    Json.Decode.succeed LocalStyle
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "cases" decodeCases))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "margin" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "titleCornerRadius" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "bodyCornerRadius" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "backgroundColour" Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "textColour" Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "titlePaddingH" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "titlePaddingV" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "bodyPaddingH" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "bodyPaddingV" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "maxLines" Json.Decode.int))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "fontFamily" Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "titleSize" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "subtitleSize" Json.Decode.float))
        |> Json.Decode.Extra.andMap (Json.Decode.maybe (Json.Decode.field "bodySize" Json.Decode.float))


encodeSize : Size -> Json.Encode.Value
encodeSize { height, width } =
    Json.Encode.object
        [ ( "height", Json.Encode.int height )
        , ( "width", Json.Encode.int width )
        ]


decodeSize : Json.Decode.Decoder Size
decodeSize =
    Json.Decode.map2 Size
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "width" Json.Decode.int)


encodeCases : StyleCases -> Json.Encode.Value
encodeCases cases =
    Json.Encode.object <|
        case cases of
            Full ->
                [ ( "type", Json.Encode.string "full" ) ]

            Horizontal { align } ->
                [ ( "type", Json.Encode.string "horizontal" )
                , ( "align", encodeVAlign align )
                ]

            Vertical { align, split } ->
                [ ( "type", Json.Encode.string "vertical" )
                , ( "align", encodeHAlign align )
                , ( "split", Json.Encode.float split )
                ]


decodeCases : Json.Decode.Decoder StyleCases
decodeCases =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "full" ->
                        Json.Decode.succeed Full

                    "horizontal" ->
                        Json.Decode.map (\align -> Horizontal { align = align })
                            (Json.Decode.field "align" decodeVAlign)

                    "vertical" ->
                        Json.Decode.map2 (\align split -> Vertical { align = align, split = split })
                            (Json.Decode.field "align" decodeHAlign)
                            (Json.Decode.field "split" Json.Decode.float)

                    _ ->
                        Json.Decode.fail "Invalid style cases type"
            )


encodeHAlign : HAlign -> Json.Encode.Value
encodeHAlign align =
    case align of
        Left ->
            Json.Encode.string "left"

        HCenter ->
            Json.Encode.string "center"

        Right ->
            Json.Encode.string "right"


decodeHAlign : Json.Decode.Decoder HAlign
decodeHAlign =
    Json.Decode.string
        |> Json.Decode.andThen
            (\align ->
                case align of
                    "left" ->
                        Json.Decode.succeed Left

                    "center" ->
                        Json.Decode.succeed HCenter

                    "right" ->
                        Json.Decode.succeed Right

                    _ ->
                        Json.Decode.fail "Invalid HAlign"
            )


encodeVAlign : VAlign -> Json.Encode.Value
encodeVAlign align =
    case align of
        Top ->
            Json.Encode.string "top"

        VCenter ->
            Json.Encode.string "center"

        Bottom ->
            Json.Encode.string "bottom"


decodeVAlign : Json.Decode.Decoder VAlign
decodeVAlign =
    Json.Decode.string
        |> Json.Decode.andThen
            (\align ->
                case align of
                    "top" ->
                        Json.Decode.succeed Top

                    "center" ->
                        Json.Decode.succeed VCenter

                    "bottom" ->
                        Json.Decode.succeed Bottom

                    _ ->
                        Json.Decode.fail "Invalid VAlign"
            )


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe f x =
    case x of
        Just y ->
            f y

        Nothing ->
            Json.Encode.null
