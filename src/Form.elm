module Form exposing (Field, FieldMaybe, field, fieldDynamic, fieldList, fieldMaybe, fieldMaybeDynamic, fieldMaybeList, fieldSelect, fieldSelectMaybe, fieldStripMaybe, float, id, input, inputMaybe, int, map, mapMaybe, mapMaybe2, maybe)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra as List
import Maybe.Extra as Maybe


type alias FieldMaybe1 a b c =
    { label : String
    , get : a -> c
    , getMaybe : b -> Maybe c
    , update : c -> a -> a
    , updateMaybe : Maybe c -> b -> b
    }


type Field a
    = FieldString
        { label : String
        , get : a -> String
        , update : String -> a -> a
        }
    | FieldSelect
        { label : String
        , cases : List { label : String, set : a -> a, isThis : a -> Bool }
        }
    | FieldList (List (Field a))
    | FieldDynamic (a -> Field a)


type FieldMaybe a b
    = FieldMaybeString
        { label : String
        , get : a -> String
        , getMaybe : b -> Maybe String
        , update : String -> a -> a
        , updateMaybe : Maybe String -> b -> b
        }
    | FieldMaybeSelect
        { label : String
        , cases : List { label : String, set : a -> a, setMaybe : b -> b, isThis : a -> Bool, isThisMaybe : b -> Maybe Bool }
        , setEmpty : b -> b
        , hasValue : b -> Bool
        }
    | FieldMaybeList (List (FieldMaybe a b))
    | FieldMaybeDynamic (a -> Maybe b -> FieldMaybe a b)


field : String -> Field String
field label =
    FieldString
        { label = label
        , get = \x -> x
        , update = \x _ -> x
        }


fieldList : List (Field a) -> Field a
fieldList =
    FieldList


fieldDynamic : (a -> Field a) -> Field a
fieldDynamic =
    FieldDynamic


fieldMaybe : String -> FieldMaybe String String
fieldMaybe label =
    FieldMaybeString
        { label = label
        , get = \x -> x
        , getMaybe = Just
        , update = \x _ -> x
        , updateMaybe = \x y -> Maybe.withDefault y x
        }


fieldMaybeList : List (FieldMaybe a b) -> FieldMaybe a b
fieldMaybeList =
    FieldMaybeList


fieldMaybeDynamic : (a -> Maybe b -> FieldMaybe a b) -> FieldMaybe a b
fieldMaybeDynamic =
    FieldMaybeDynamic


type alias Mapping a b =
    ( b -> a, (a -> a) -> (b -> b) )


type alias MappingMaybe a b =
    ( b -> Maybe a, (a -> a) -> (b -> b) )


id : Mapping a a
id =
    ( \x -> x, \f -> f )


int : Mapping String Int
int =
    ( String.fromInt, \f x -> Maybe.withDefault x <| String.toInt <| f <| String.fromInt x )


float : Mapping String Float
float =
    ( String.fromFloat, \f x -> Maybe.withDefault x <| String.toFloat <| f <| String.fromFloat x )


maybe : Mapping a b -> MappingMaybe a (Maybe b)
maybe ( f, g ) =
    ( Maybe.map f, Maybe.map << g )


map : Mapping a b -> Field a -> Field b
map ( f, g ) field1 =
    case field1 of
        FieldString { label, get, update } ->
            FieldString
                { label = label
                , get = get << f
                , update = g << update
                }

        FieldSelect field2 ->
            FieldSelect
                { label = field2.label
                , cases =
                    List.map
                        (\{ label, set, isThis } ->
                            { label = label
                            , set = g set
                            , isThis = isThis << f
                            }
                        )
                        field2.cases
                }

        FieldList fields ->
            FieldList <| List.map (map ( f, g )) fields

        FieldDynamic h ->
            FieldDynamic (map ( f, g ) << h << f)


mapMaybe : Mapping a c -> MappingMaybe b d -> FieldMaybe a b -> FieldMaybe c d
mapMaybe ( f1, f2 ) ( f3, f4 ) field1 =
    case field1 of
        FieldMaybeString { label, get, getMaybe, update, updateMaybe } ->
            FieldMaybeString
                { label = label
                , get = get << f1
                , getMaybe = Maybe.andThen getMaybe << f3
                , update = f2 << update
                , updateMaybe = f4 << updateMaybe
                }

        FieldMaybeSelect ({ cases, setEmpty, hasValue } as field2) ->
            FieldMaybeSelect
                { label = field2.label
                , cases =
                    List.map
                        (\{ label, set, setMaybe, isThis, isThisMaybe } ->
                            { label = label
                            , set = f2 set
                            , setMaybe = f4 setMaybe
                            , isThis = isThis << f1
                            , isThisMaybe = Maybe.andThen isThisMaybe << f3
                            }
                        )
                        cases
                , setEmpty = f4 setEmpty
                , hasValue = Maybe.unwrap False hasValue << f3
                }

        FieldMaybeList fields ->
            FieldMaybeList <| List.map (mapMaybe ( f1, f2 ) ( f3, f4 )) fields

        FieldMaybeDynamic g ->
            FieldMaybeDynamic <| \x y -> mapMaybe ( f1, f2 ) ( f3, f4 ) <| g (f1 x) (Maybe.andThen f3 y)


mapMaybe2 : Mapping a b -> FieldMaybe a a -> FieldMaybe b (Maybe b)
mapMaybe2 f =
    mapMaybe f (maybe f)


fieldStripMaybe : FieldMaybe a b -> Field a
fieldStripMaybe field1 =
    case field1 of
        FieldMaybeString { label, get, update } ->
            FieldString { label = label, get = get, update = update }

        FieldMaybeSelect { label, cases } ->
            FieldSelect { label = label, cases = List.map (\case_ -> { label = case_.label, set = case_.set, isThis = case_.isThis }) cases }

        FieldMaybeList fields ->
            FieldList <| List.map fieldStripMaybe fields

        FieldMaybeDynamic f ->
            FieldDynamic <| \x -> fieldStripMaybe <| f x Nothing


fieldSelect : String -> List { label : String, default : a, isThis : a -> Bool } -> Field a
fieldSelect label cases =
    FieldSelect
        { label = label
        , cases =
            List.map
                (\case_ ->
                    { label = case_.label
                    , set = \_ -> case_.default
                    , isThis = case_.isThis
                    }
                )
                cases
        }


fieldSelectMaybe : String -> List { label : String, default : a, isThis : a -> Bool } -> FieldMaybe a (Maybe a)
fieldSelectMaybe label cases =
    FieldMaybeSelect
        { label = label
        , cases =
            List.map
                (\case_ ->
                    { label = case_.label
                    , set = \_ -> case_.default
                    , setMaybe = \_ -> Just case_.default
                    , isThis = case_.isThis
                    , isThisMaybe = Maybe.map case_.isThis
                    }
                )
                cases
        , setEmpty = \_ -> Nothing
        , hasValue = Maybe.isJust
        }


input : a -> (a -> msg) -> Field a -> Html msg
input x send field1 =
    case field1 of
        FieldString { label, get, update } ->
            inputRaw label (get x) (\y -> send <| update y x)

        FieldSelect { label, cases } ->
            inputSelect label cases x send

        FieldList fields ->
            Html.div [] <| List.map (input x send) fields

        FieldDynamic f ->
            input x send <| f x


inputMaybe : a -> b -> (b -> msg) -> FieldMaybe a b -> Html msg
inputMaybe x y send field1 =
    case field1 of
        FieldMaybeString { label, get, getMaybe, updateMaybe } ->
            inputMaybeRaw label (get x) (getMaybe y) (\z -> send <| updateMaybe z y)

        FieldMaybeSelect { label, cases, setEmpty, hasValue } ->
            inputSelectMaybe label cases setEmpty hasValue x y send

        FieldMaybeList fields ->
            Html.div [] <| List.map (inputMaybe x y send) fields

        FieldMaybeDynamic f ->
            inputMaybe x y send <| f x <| Just y


inputRaw : String -> String -> (String -> msg) -> Html msg
inputRaw label value onInput =
    Html.div [ Html.Attributes.style "margin" "3px" ]
        [ Html.label [ Html.Attributes.class "form-label" ] [ Html.text label ]
        , Html.input
            [ Html.Attributes.class "form-control"
            , Html.Attributes.value value
            , Html.Events.onInput onInput
            ]
            []
        ]


inputMaybeRaw : String -> String -> Maybe String -> (Maybe String -> msg) -> Html msg
inputMaybeRaw label default value onInput =
    Html.div [ Html.Attributes.style "margin" "3px" ]
        [ Html.label [ Html.Attributes.class "form-label" ] [ Html.text label ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            , Html.Attributes.style "align-items" "center"
            ]
            [ Html.input
                [ Html.Attributes.class "form-control"
                , Html.Attributes.style "border-top-right-radius" "0px"
                , Html.Attributes.style "border-bottom-right-radius" "0px"
                , Html.Attributes.value <| Maybe.withDefault default value
                , Html.Events.onInput (onInput << Just)
                , Html.Attributes.style "flex" "1"
                ]
                []
            , resetButton (Maybe.isJust value) (onInput Nothing)
            ]
        ]


inputSelect : String -> List { label : String, set : a -> a, isThis : a -> Bool } -> a -> (a -> msg) -> Html msg
inputSelect label cases value onInput =
    Html.div [ Html.Attributes.style "margin" "3px" ]
        [ Html.label [ Html.Attributes.class "form-label" ] [ Html.text label ]
        , Html.select
            [ Html.Attributes.class "form-select"
            , Html.Events.onInput <|
                onInput
                    << Maybe.withDefault value
                    << Maybe.map (\val -> val.set value)
                    << Maybe.andThen (\index -> List.getAt index cases)
                    << String.toInt
            ]
          <|
            List.indexedMap
                (\index option ->
                    Html.option
                        [ Html.Attributes.value (String.fromInt index)
                        , Html.Attributes.selected (option.isThis value)
                        ]
                        [ Html.text option.label ]
                )
                cases
        ]


inputSelectMaybe : String -> List { label : String, set : a -> a, setMaybe : b -> b, isThis : a -> Bool, isThisMaybe : b -> Maybe Bool } -> (b -> b) -> (b -> Bool) -> a -> b -> (b -> msg) -> Html msg
inputSelectMaybe label cases setEmpty hasValue default value onInput =
    Html.div [ Html.Attributes.style "margin" "3px" ]
        [ Html.label [ Html.Attributes.class "form-label" ] [ Html.text label ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            ]
            [ Html.select
                [ Html.Attributes.class "form-select"
                , Html.Attributes.style "border-top-right-radius" "0px"
                , Html.Attributes.style "border-bottom-right-radius" "0px"
                , Html.Events.onInput <|
                    \indexString ->
                        case String.toInt indexString |> Maybe.andThen (\index -> List.getAt index cases) of
                            Nothing ->
                                onInput value

                            Just value_ ->
                                onInput <| value_.setMaybe value
                ]
              <|
                List.indexedMap
                    (\index case_ ->
                        Html.option
                            [ Html.Attributes.value (String.fromInt index)
                            , Html.Attributes.selected <| Maybe.withDefault (case_.isThis default) <| case_.isThisMaybe value
                            ]
                            [ Html.text case_.label ]
                    )
                    cases
            , resetButton (hasValue value) (onInput <| setEmpty value)
            ]
        ]


resetButton : Bool -> msg -> Html msg
resetButton highlighted onInput =
    Html.div
        [ Html.Attributes.class "btn"
        , if highlighted then
            Html.Attributes.class "btn-outline-primary"

          else
            Html.Attributes.class "btn-outline-secondary"
        , Html.Attributes.style "border-top-left-radius" "0px"
        , Html.Attributes.style "border-bottom-left-radius" "0px"
        , Html.Events.onClick onInput
        ]
        [ Html.text "↻" ]
