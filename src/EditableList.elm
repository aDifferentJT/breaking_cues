module EditableList exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Task


type alias Element value msg =
    { id : String
    , value : value
    , view :
        { topLevelAttributes : List (Html.Attribute msg)
        , dragHandleAttributes : List (Html.Attribute msg)
        }
        -> Html msg
    }


type alias ElementInfo =
    { criticalYPosition : Float
    , height : Float
    }


type alias CurrentlyDragging value msg =
    { id : String
    , value : value
    , elements : List (Element value msg)
    , elementsInfo : Dict String ElementInfo
    , idsInitiallyBeforeSelected : List String
    , initialPageY : Float
    , pageY : Float
    }


type Model value msg
    = Model
        { wrapMsg : Msg value msg -> msg
        , batchMsgs : List msg -> msg
        , listReordered : List value -> msg
        , currentlyDragging : Maybe (CurrentlyDragging value msg)
        }


type Msg value msg
    = NOP
    | StartDragging { selectedIndex : Int, id : String, value : value, elements : List (Element value msg), pageY : Float }
    | GotElementInfo { id : String, elementInfo : ElementInfo }
    | StopDragging
    | MouseMoved { pageY : Float }


init : { wrapMsg : Msg value msg -> msg, batchMsgs : List msg -> msg, listReordered : List value -> msg } -> Model value msg
init { wrapMsg, batchMsgs, listReordered } =
    Model
        { wrapMsg = wrapMsg
        , batchMsgs = batchMsgs
        , listReordered = listReordered
        , currentlyDragging = Nothing
        }


view : Model value msg -> String -> List (Html.Attribute msg) -> List (Element value msg) -> Html msg
view (Model ({ wrapMsg } as model)) name ulAttributes elements =
    let
        viewElement extraAttributes index element =
            ( element.id
            , element.view
                { topLevelAttributes =
                    [ Html.Attributes.id element.id ] ++ extraAttributes
                , dragHandleAttributes =
                    [ Html.Events.preventDefaultOn "mousedown" <|
                        Json.Decode.map (\pageY -> ( wrapMsg <| StartDragging { selectedIndex = index, id = element.id, value = element.value, elements = elements, pageY = pageY }, True )) <|
                            Json.Decode.field "pageY" Json.Decode.float
                    ]
                }
            )
    in
    Html.Keyed.node name ulAttributes <|
        case model.currentlyDragging of
            Nothing ->
                List.indexedMap (viewElement []) elements

            Just ({ elementsInfo, idsInitiallyBeforeSelected, initialPageY, pageY } as currentlyDragging) ->
                case
                    ( elementsWithInfo currentlyDragging elements
                    , Maybe.combine <|
                        List.map
                            (\id -> Maybe.map (\elementInfo -> elementInfo.height) <| Dict.get id elementsInfo)
                            idsInitiallyBeforeSelected
                    )
                of
                    ( Just elementsWithInfo_, Just heightsInitallyBeforeSelected ) ->
                        let
                            elementsNowBeforeSelected =
                                elementsBeforeSelected currentlyDragging elementsWithInfo_

                            elementsNowAfterSelected =
                                elementsAfterSelected currentlyDragging elementsWithInfo_

                            totalHeightInitallyBeforeSelected =
                                List.sum heightsInitallyBeforeSelected

                            totalHeightNowBeforeSelected =
                                List.sum <| List.map (\( _, elementInfo ) -> elementInfo.height) elementsNowBeforeSelected
                        in
                        List.concat
                            [ List.indexedMap (viewElement []) <| List.map (\( element, _ ) -> element) elementsNowBeforeSelected
                            , List.indexedMap
                                (viewElement
                                    [ Html.Attributes.style "z-index" "1"
                                    , Html.Attributes.style "position" "relative"
                                    , Html.Attributes.style "top" (String.fromFloat (pageY - initialPageY + totalHeightInitallyBeforeSelected - totalHeightNowBeforeSelected) ++ "px")
                                    ]
                                )
                              <|
                                List.filter
                                    (\element -> currentlyDragging.id == element.id)
                                    elements
                            , List.indexedMap (viewElement []) <| List.map (\( element, _ ) -> element) elementsNowAfterSelected
                            ]

                    _ ->
                        List.indexedMap (viewElement []) elements


elementsWithInfo : CurrentlyDragging value msg -> List (Element value msg) -> Maybe (List ( Element value msg, ElementInfo ))
elementsWithInfo { elementsInfo } elements =
    Maybe.combine <|
        List.map
            (\element -> Maybe.map (\elementInfo -> ( element, elementInfo )) <| Dict.get element.id elementsInfo)
            elements


elementsBeforeSelected : CurrentlyDragging value msg -> List ( Element value msg, ElementInfo ) -> List ( Element value msg, ElementInfo )
elementsBeforeSelected currentlyDragging =
    List.filter (\( element, elementInfo ) -> currentlyDragging.id /= element.id && elementInfo.criticalYPosition < currentlyDragging.pageY)


elementsAfterSelected : CurrentlyDragging value msg -> List ( Element value msg, ElementInfo ) -> List ( Element value msg, ElementInfo )
elementsAfterSelected currentlyDragging =
    List.filter (\( element, elementInfo ) -> currentlyDragging.id /= element.id && elementInfo.criticalYPosition >= currentlyDragging.pageY)


update : Msg value msg -> Model value msg -> ( Model value msg, Cmd msg )
update msg (Model ({ wrapMsg } as model)) =
    case msg of
        NOP ->
            ( Model model, Cmd.none )

        StartDragging { selectedIndex, id, value, elements, pageY } ->
            let
                ids =
                    List.map (\element -> element.id) elements
            in
            ( Model
                { model
                    | currentlyDragging =
                        Just
                            { id = id
                            , value = value
                            , elements = elements
                            , elementsInfo = Dict.empty
                            , idsInitiallyBeforeSelected = List.takeWhile ((/=) id) ids
                            , initialPageY = pageY
                            , pageY = pageY
                            }
                }
            , Cmd.batch <|
                List.indexedMap
                    (\index id_ ->
                        Task.attempt
                            (\res ->
                                case res of
                                    Ok { element } ->
                                        wrapMsg
                                            (GotElementInfo
                                                { id = id_
                                                , elementInfo =
                                                    { criticalYPosition =
                                                        if index < selectedIndex then
                                                            element.y + element.height

                                                        else
                                                            element.y
                                                    , height = element.height
                                                    }
                                                }
                                            )

                                    Err _ ->
                                        wrapMsg NOP
                            )
                            (Browser.Dom.getElement id_)
                    )
                    ids
            )

        GotElementInfo { id, elementInfo } ->
            case model.currentlyDragging of
                Just currentlyDragging ->
                    ( Model { model | currentlyDragging = Just { currentlyDragging | elementsInfo = Dict.insert id elementInfo currentlyDragging.elementsInfo } }, Cmd.none )

                Nothing ->
                    ( Model model, Cmd.none )

        StopDragging ->
            ( Model { model | currentlyDragging = Nothing }, Cmd.none )

        MouseMoved { pageY } ->
            case model.currentlyDragging of
                Nothing ->
                    ( Model model, Cmd.none )

                Just currentlyDragging ->
                    ( Model { model | currentlyDragging = Just { currentlyDragging | pageY = pageY } }, Cmd.none )


subscriptions : Model value msg -> Sub msg
subscriptions (Model ({ wrapMsg, batchMsgs, listReordered } as model)) =
    case model.currentlyDragging of
        Nothing ->
            Sub.none

        Just currentlyDragging ->
            Sub.batch
                [ Browser.Events.onMouseUp <|
                    Json.Decode.lazy <|
                        \() ->
                            Json.Decode.succeed <|
                                batchMsgs <|
                                    List.concat
                                        [ [ wrapMsg StopDragging ]
                                        , case elementsWithInfo currentlyDragging currentlyDragging.elements of
                                            Just elementsWithInfo_ ->
                                                [ listReordered <|
                                                    List.concat
                                                        [ List.map (\( element, _ ) -> element.value) <| elementsBeforeSelected currentlyDragging elementsWithInfo_
                                                        , [ currentlyDragging.value ]
                                                        , List.map (\( element, _ ) -> element.value) <| elementsAfterSelected currentlyDragging elementsWithInfo_
                                                        ]
                                                ]

                                            Nothing ->
                                                []
                                        ]
                , Browser.Events.onMouseMove <| Json.Decode.map (\pageY -> wrapMsg (MouseMoved { pageY = pageY })) <| Json.Decode.field "pageY" Json.Decode.float
                ]
