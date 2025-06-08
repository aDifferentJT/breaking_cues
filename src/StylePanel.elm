module StylePanel exposing (localStylePanel, outputPanel)

import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Programme
import Style exposing (..)


outputPanel : { output : Programme.Output, updateOutput : Programme.Output -> msg } -> Html msg
outputPanel { output, updateOutput } =
    Html.form [ Html.Attributes.style "margin" "6px" ] <|
        [ Form.input output updateOutput <|
            Form.fieldList
                [ Form.map ( .name, \f o -> { o | name = f o.name } ) <|
                    Form.field "Name"
                , Form.map ( .style, \f o -> { o | style = f o.style } ) <|
                    Form.fieldList <|
                        [ Form.map ( .size, \f s -> { s | size = f s.size } ) <|
                            Form.fieldList
                                [ Form.map ( .width, \f s -> { s | width = f s.width } ) <|
                                    Form.map Form.int <|
                                        Form.field "Width"
                                , Form.map ( .height, \f s -> { s | height = f s.height } ) <|
                                    Form.map Form.int <|
                                        Form.field "Height"
                                ]
                        , Form.map ( .cases, \f s -> { s | cases = f s.cases } ) <|
                            Form.fieldList
                                [ Form.fieldSelect "Style" styleCases
                                , caseSpecificFields
                                ]
                        ]
                            ++ List.map Form.fieldStripMaybe commonFields
                ]
        ]


localStylePanel : { default : Style, style : LocalStyle, updateStyle : LocalStyle -> msg } -> Html msg
localStylePanel { default, style, updateStyle } =
    Html.form [ Html.Attributes.style "margin" "6px" ] <|
        [ Form.inputMaybe default style updateStyle <| Form.mapMaybe ( .cases, \f s -> { s | cases = f s.cases } ) ( .cases, \f s -> { s | cases = f s.cases } ) <| Form.fieldSelectMaybe "Style" styleCases
        , Form.input style updateStyle <|
            case style.cases of
                Nothing ->
                    Form.fieldList []

                Just cases_ ->
                    Form.map ( \_ -> cases_, \f s -> { s | cases = Just <| f cases_ } ) caseSpecificFields
        ]
            ++ List.map (Form.inputMaybe default style updateStyle) commonFields


caseSpecificFields : Form.Field StyleCases
caseSpecificFields =
    Form.fieldDynamic <|
        \cases ->
            case cases of
                Style.Full ->
                    Form.fieldList
                        []

                Style.Horizontal { align } ->
                    Form.map ( \_ -> align, \f _ -> Style.Horizontal { align = f align } ) <|
                        Form.fieldSelect "Align"
                            [ { label = "Top"
                              , default = Style.Top
                              , isThis = (==) Style.Top
                              }
                            , { label = "Center"
                              , default = Style.VCenter
                              , isThis = (==) Style.VCenter
                              }
                            , { label = "Bottom"
                              , default = Style.Bottom
                              , isThis = (==) Style.Bottom
                              }
                            ]

                Style.Vertical { align, split } ->
                    Form.fieldList
                        [ Form.map ( \_ -> align, \f _ -> Style.Vertical { align = f align, split = split } ) <|
                            Form.fieldSelect "Align"
                                [ { label = "Left"
                                  , default = Style.Left
                                  , isThis = (==) Style.Left
                                  }
                                , { label = "Center"
                                  , default = Style.HCenter
                                  , isThis = (==) Style.HCenter
                                  }
                                , { label = "Right"
                                  , default = Style.Right
                                  , isThis = (==) Style.Right
                                  }
                                ]
                        , Form.map ( \_ -> split, \f _ -> Style.Vertical { align = align, split = f split } ) <|
                            Form.map Form.float <|
                                Form.field "Split"
                        ]


commonFields : List (Form.FieldMaybe Style LocalStyle)
commonFields =
    [ Form.mapMaybe ( .margin, \f s -> { s | margin = f s.margin } ) ( .margin, \f s -> { s | margin = f s.margin } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Margin"
    , Form.mapMaybe ( .titleCornerRadius, \f s -> { s | titleCornerRadius = f s.titleCornerRadius } ) ( .titleCornerRadius, \f s -> { s | titleCornerRadius = f s.titleCornerRadius } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Title Corner Radius"
    , Form.mapMaybe ( .bodyCornerRadius, \f s -> { s | bodyCornerRadius = f s.bodyCornerRadius } ) ( .bodyCornerRadius, \f s -> { s | bodyCornerRadius = f s.bodyCornerRadius } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Body Corner Radius"
    , Form.mapMaybe ( .backgroundColour, \f s -> { s | backgroundColour = f s.backgroundColour } ) ( .backgroundColour, \f s -> { s | backgroundColour = f s.backgroundColour } ) <| Form.fieldMaybe "Background Colour"
    , Form.mapMaybe ( .textColour, \f s -> { s | textColour = f s.textColour } ) ( .textColour, \f s -> { s | textColour = f s.textColour } ) <| Form.fieldMaybe "Text Colour"
    , Form.mapMaybe ( .titlePaddingH, \f s -> { s | titlePaddingH = f s.titlePaddingH } ) ( .titlePaddingH, \f s -> { s | titlePaddingH = f s.titlePaddingH } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Title Horizontal Padding"
    , Form.mapMaybe ( .titlePaddingV, \f s -> { s | titlePaddingV = f s.titlePaddingV } ) ( .titlePaddingV, \f s -> { s | titlePaddingV = f s.titlePaddingV } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Title Vertical Padding"
    , Form.mapMaybe ( .bodyPaddingH, \f s -> { s | bodyPaddingH = f s.bodyPaddingH } ) ( .bodyPaddingH, \f s -> { s | bodyPaddingH = f s.bodyPaddingH } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Body Horizontal Padding"
    , Form.mapMaybe ( .bodyPaddingV, \f s -> { s | bodyPaddingV = f s.bodyPaddingV } ) ( .bodyPaddingV, \f s -> { s | bodyPaddingV = f s.bodyPaddingV } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Body Vertical Padding"
    , Form.mapMaybe ( .maxLines, \f s -> { s | maxLines = f s.maxLines } ) ( .maxLines, \f s -> { s | maxLines = f s.maxLines } ) <| Form.mapMaybe Form.int Form.maybeInt <| Form.fieldMaybe "Max Lines"
    , Form.mapMaybe ( .fontFamily, \f s -> { s | fontFamily = f s.fontFamily } ) ( .fontFamily, \f s -> { s | fontFamily = f s.fontFamily } ) <| Form.fieldMaybe "Font Family"
    , Form.mapMaybe ( .titleSize, \f s -> { s | titleSize = f s.titleSize } ) ( .titleSize, \f s -> { s | titleSize = f s.titleSize } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Title Size"
    , Form.mapMaybe ( .subtitleSize, \f s -> { s | subtitleSize = f s.subtitleSize } ) ( .subtitleSize, \f s -> { s | subtitleSize = f s.subtitleSize } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Subtitle Size"
    , Form.mapMaybe ( .bodySize, \f s -> { s | bodySize = f s.bodySize } ) ( .bodySize, \f s -> { s | bodySize = f s.bodySize } ) <| Form.mapMaybe Form.float Form.maybeFloat <| Form.fieldMaybe "Body Size"
    ]


styleCases : List { label : String, default : Style.StyleCases, isThis : Style.StyleCases -> Bool }
styleCases =
    [ { label = "Full"
      , default = Style.Full
      , isThis =
            \case_ ->
                case case_ of
                    Style.Full ->
                        True

                    _ ->
                        False
      }
    , { label = "Horizontal"
      , default = Style.Horizontal { align = Style.Bottom }
      , isThis =
            \case_ ->
                case case_ of
                    Style.Horizontal {} ->
                        True

                    _ ->
                        False
      }
    , { label = "Vertical"
      , default = Style.Vertical { align = Style.Right, split = 0.5 }
      , isThis =
            \case_ ->
                case case_ of
                    Style.Vertical {} ->
                        True

                    _ ->
                        False
      }
    ]
