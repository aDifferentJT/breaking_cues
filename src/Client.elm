port module Client exposing (main)

import Basics.Extra exposing (curry, flip, uncurry)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Deck exposing (Deck)
import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)
import Draggable
import Draggable.Events
import EditableList
import Fetch.Bible
import Fetch.Common exposing (errorToString)
import Fetch.Hymn
import Fetch.Psalm
import Form
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import List.Cartesian
import List.Extra as List
import Output
import Programme exposing (Programme)
import Result.Extra as Result
import Slide exposing (Slide)
import Style exposing (LocalStyle, Style)
import StylePanel
import Task
import Url exposing (Url)
import Url.Builder


port chunkSplitSend : Json.Encode.Value -> Cmd msg


port chunkSplitRecv : (Json.Decode.Value -> msg) -> Sub msg


port programmeSend : Json.Encode.Value -> Cmd msg


port programmeRecv : (Json.Decode.Value -> msg) -> Sub msg


port open : () -> Cmd msg


port save : String -> Cmd msg


type alias Flags =
    { tauri : Bool
    }


type ProgrammeTab
    = Programme
    | Outputs { output : Int }
    | Templates
    | Errors


type PreviewTab
    = Preview
    | Edit { list : EditableList.Model Deck.Chunk Msg }
    | Fetch FetchTab
    | Style { output : Int }


type FetchTab
    = Search (List (Int -> Deck))
    | Bible Fetch.Bible.Params
    | Psalm Fetch.Psalm.Params
    | Hymn Fetch.Hymn.Params


type alias ChunkToSlides =
    AnyDict ( Deck.ComparableChunk, Style.ComparableStyle ) ( Deck.Chunk, Style ) (List Slide)


type alias Model =
    { tauri : Bool
    , chunkToSlides : ChunkToSlides
    , programme : Programme
    , programmeTab : ProgrammeTab
    , programmeList : EditableList.Model Deck Msg
    , previewSlide : Int
    , previewSelected : Maybe Selected
    , previewPreview : PreviewPanelModel
    , previewTab : PreviewTab
    , live : Maybe Output.LiveModel
    , liveOutput : Output.Model {}
    , livePreview : PreviewPanelModel
    , errors : List String
    }


type alias Selected =
    { id : Int, variant : Int }


type alias PreviewPanelModel =
    { height : Float
    , dragState : Draggable.State ()
    , output : String
    , elementId : String
    , availableWidth : Float
    }


type Msg
    = Batch (List Msg)
    | RunCmd (Cmd Msg)
    | ChunkSplitRecv ( Deck.Chunk, Style ) (List Slide)
    | ProgrammeRecv Programme
    | RefreshSlides
    | UpdateOutputs (List Programme.Output)
    | UpdateDecks (List Deck)
    | AddDeck Deck
    | RemoveDeck Int
    | UpdateDeck Int Deck
    | UpdateDeckTitle Int String
    | AddVariant Int Deck.Variant
    | RemoveVariant Int Int
    | UpdateVariant Int Int Deck.Variant
    | UpdateVariantName Int Int String
    | UpdateVariantStyle Int Int String LocalStyle
    | AddChunk Int Int Deck.Chunk
    | RemoveChunk Int Int Int
    | UpdateChunk Int Int Int Deck.Chunk
    | UpdateDefaultVariant Int Int
    | OnResize
    | ProgrammeSwitchTab ProgrammeTab
    | ProgrammeList (EditableList.Msg Deck Msg)
    | PreviewShowSlide Int
    | PreviewSelect (Maybe Selected)
    | PreviewPreviewMsg PreviewPanelMsg
    | PreviewSwitchTab PreviewTab
    | EditChunkList (EditableList.Msg Deck.Chunk Msg)
    | LiveRecv (Maybe Output.LiveModel)
    | LiveSend (Maybe Output.LiveModel)
    | LiveOutput Output.Msg
    | LivePreviewMsg PreviewPanelMsg
    | ShowErr String
    | RemoveErr Int


type PreviewPanelMsg
    = PreviewPanelDragBy Draggable.Delta
    | PreviewPanelDragMsg (Draggable.Msg ())
    | PreviewPanelSelectOutput String
    | UpdatePreviewPanelSize Size


type alias Size =
    { width : Float, height : Float }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { tauri } =
    ( { tauri = tauri
      , chunkToSlides = Dict.Any.empty (\( chunk, style ) -> ( Deck.chunkToComparable chunk, Style.styleToComparable style ))
      , programme =
            { decks = []
            , outputs = [ { name = "Broadcast", style = Style.default } ]
            }
      , programmeTab = Programme
      , programmeList = EditableList.init { wrapMsg = ProgrammeList, batchMsgs = Batch, listReordered = UpdateDecks }
      , previewSlide = 0
      , previewSelected = Nothing
      , previewPreview =
            { height = 50
            , dragState = Draggable.init
            , output = ""
            , elementId = "previewPreviewPanel"
            , availableWidth = 0
            }
      , previewTab = Preview
      , live = Nothing
      , liveOutput = Output.init Nothing
      , livePreview =
            { height = 50
            , dragState = Draggable.init
            , output = ""
            , elementId = "livePreviewPanel"
            , availableWidth = 0
            }
      , errors = []
      }
    , Task.perform
        (\_ -> OnResize)
        Browser.Dom.getViewport
    )


view : Model -> Browser.Document Msg
view { tauri, programme, programmeTab, programmeList, previewSlide, previewSelected, previewPreview, previewTab, live, liveOutput, livePreview, errors } =
    { title = "Breaking Cues"
    , body =
        [ Html.div
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "display" "flex"
            ]
            [ Html.div
                [ Html.Attributes.class "card"
                , Html.Attributes.style "margin" "3px"
                , Html.Attributes.style "flex" "1"
                ]
                (tabBar
                    [ { msg = ProgrammeSwitchTab Programme
                      , label = Html.text "Programme"
                      , active = programmeTab == Programme
                      }
                    , { msg = ProgrammeSwitchTab (Outputs { output = 0 })
                      , label = Html.text "Outputs"
                      , active =
                            case programmeTab of
                                Outputs _ ->
                                    True

                                _ ->
                                    False
                      }
                    , { msg = ProgrammeSwitchTab Templates
                      , label = Html.text "Templates"
                      , active = programmeTab == Templates
                      }
                    , { msg = ProgrammeSwitchTab Errors
                      , label = Html.text "Errors"
                      , active = programmeTab == Errors
                      }
                    ]
                    :: (case programmeTab of
                            Programme ->
                                [ Html.div
                                    [ Html.Attributes.class "card-header"
                                    , Html.Attributes.style "display" "flex"
                                    , Html.Attributes.style "flex-direction" "row"
                                    , Html.Attributes.style "align-items" "center"
                                    , Html.Attributes.style "overflow-x" "scroll"
                                    ]
                                    [ Html.div [ Html.Attributes.style "flex" "1" ] [ Html.text "Programme" ]
                                    , Html.div
                                        [ Html.Attributes.class "btn"
                                        , Html.Attributes.class "btn-outline-primary"
                                        , Html.Attributes.class "bi-arrow-repeat"
                                        , Html.Attributes.style "margin-left" "4px"
                                        , Html.Attributes.style "margin-right" "4px"
                                        , Html.Events.onClick <| RefreshSlides
                                        ]
                                        []
                                    , Html.div
                                        [ Html.Attributes.class "btn"
                                        , Html.Attributes.class "btn-outline-primary"
                                        , Html.Attributes.class "bi-folder"
                                        , Html.Attributes.style "margin-left" "4px"
                                        , Html.Attributes.style "border-top-right-radius" "0px"
                                        , Html.Attributes.style "border-bottom-right-radius" "0px"
                                        , Html.Events.onClick <| RunCmd <| open ()
                                        ]
                                        []
                                    , Html.a
                                        [ Html.Attributes.class "btn"
                                        , Html.Attributes.class "btn-outline-primary"
                                        , Html.Attributes.class "bi-floppy"
                                        , Html.Attributes.style "margin-right" "4px"
                                        , Html.Attributes.style "border-top-left-radius" "0px"
                                        , Html.Attributes.style "border-bottom-left-radius" "0px"
                                        , Html.Attributes.style "border-left" "none"
                                        , Html.Attributes.download "Programme.bcp"
                                        , let
                                            data =
                                                Json.Encode.encode 0 <| Programme.encode programme
                                          in
                                          if tauri then
                                            Html.Events.onClick <| RunCmd <| save data

                                          else
                                            Html.Attributes.href <| "data:," ++ Url.percentEncode data
                                        ]
                                        []
                                    , Html.div
                                        [ Html.Attributes.class "btn"
                                        , Html.Attributes.class "btn-outline-primary"
                                        , Html.Attributes.class "bi-plus-lg"
                                        , Html.Attributes.style "margin-left" "4px"
                                        , Html.Attributes.style "padding-right" "10px"
                                        , Html.Attributes.style "border-top-right-radius" "0px"
                                        , Html.Attributes.style "border-bottom-right-radius" "0px"
                                        , Html.Events.onClick <| AddDeck <| Deck.new <| (+) 1 <| List.foldl max 0 <| List.map (\deck -> deck.id) <| programme.decks
                                        ]
                                        []
                                    , Html.div
                                        [ Html.Attributes.class "btn"
                                        , Html.Attributes.class "btn-outline-primary"
                                        , Html.Attributes.class "bi-caret-down-fill"
                                        , Html.Attributes.style "padding-left" "4px"
                                        , Html.Attributes.style "padding-right" "4px"
                                        , Html.Attributes.style "border-top-left-radius" "0px"
                                        , Html.Attributes.style "border-bottom-left-radius" "0px"
                                        , Html.Attributes.style "border-left" "none"
                                        ]
                                        []
                                    ]
                                , EditableList.view programmeList
                                    "ul"
                                    [ Html.Attributes.class "list-group"
                                    , Html.Attributes.class "list-group-flush"
                                    , Html.Attributes.style "flex" "1"
                                    , Html.Attributes.style "overflow-y" "scroll"
                                    ]
                                    (programme.decks
                                        |> List.map
                                            (\deck ->
                                                { id = "programmeDeck" ++ String.fromInt deck.id
                                                , value = deck
                                                , view =
                                                    \{ topLevelAttributes, dragHandleAttributes } ->
                                                        let
                                                            active : Bool
                                                            active =
                                                                hasId previewSelected deck
                                                        in
                                                        Html.li
                                                            ([ Html.Attributes.class "list-group-item"
                                                             , Html.Attributes.classList [ ( "active", active ) ]
                                                             , Html.Events.onClick (PreviewSelect (Just { id = deck.id, variant = deck.defaultVariant }))
                                                             , Html.Attributes.style "display" "flex"
                                                             , Html.Attributes.style "flex-direction" "row"
                                                             , Html.Attributes.style "align-items" "center"

                                                             --, Html.Attributes.style "overflow-x" "scroll"
                                                             ]
                                                                ++ topLevelAttributes
                                                            )
                                                            [ Html.div [ Html.Attributes.style "flex" "1" ] [ Html.text (Deck.getTitle deck) ]
                                                            , Html.div
                                                                ([ Html.Attributes.class "btn"
                                                                 , Html.Attributes.class
                                                                    (if active then
                                                                        "btn-secondary"

                                                                     else
                                                                        "btn-outline-secondary"
                                                                    )
                                                                 , Html.Attributes.class "bi-arrow-down-up"
                                                                 , Html.Attributes.style "border-top-right-radius" "0px"
                                                                 , Html.Attributes.style "border-bottom-right-radius" "0px"
                                                                 ]
                                                                    ++ dragHandleAttributes
                                                                )
                                                                []
                                                            , Html.div
                                                                [ Html.Attributes.class "btn"
                                                                , Html.Attributes.class
                                                                    (if active then
                                                                        "btn-danger"

                                                                     else
                                                                        "btn-outline-danger"
                                                                    )
                                                                , Html.Attributes.class "bi-x-lg"
                                                                , Html.Attributes.style "border-top-left-radius" "0px"
                                                                , Html.Attributes.style "border-bottom-left-radius" "0px"
                                                                , Html.Events.stopPropagationOn "click" <| Json.Decode.succeed ( RemoveDeck deck.id, True )
                                                                ]
                                                                []
                                                            ]
                                                }
                                            )
                                    )
                                ]

                            Outputs { output } ->
                                [ Html.div
                                    [ Html.Attributes.class "card"
                                    , Html.Attributes.style "margin" "3px"
                                    , Html.Attributes.style "overflow-y" "scroll"
                                    , Html.Attributes.style "flex" "1"
                                    ]
                                    (tabBar
                                        (List.indexedMap
                                            (\index { name } -> { msg = ProgrammeSwitchTab (Outputs { output = index }), label = Html.text name, active = output == index })
                                            programme.outputs
                                            ++ [ { msg =
                                                    let
                                                        newOutputId =
                                                            List.length programme.outputs
                                                    in
                                                    Batch
                                                        [ UpdateOutputs <| programme.outputs ++ [ { name = "Output " ++ (newOutputId + 1 |> String.fromInt), style = Style.default } ]
                                                        , ProgrammeSwitchTab <| Outputs { output = newOutputId }
                                                        ]
                                                 , label = Html.text "+"
                                                 , active = False
                                                 }
                                               ]
                                        )
                                        :: (case List.getAt output programme.outputs of
                                                Nothing ->
                                                    []

                                                Just outputStyle ->
                                                    [ StylePanel.outputPanel
                                                        { output = outputStyle
                                                        , updateOutput = \newOutput -> UpdateOutputs <| List.setAt output newOutput programme.outputs
                                                        }
                                                    , Html.div
                                                        [ Html.Attributes.class "btn"
                                                        , Html.Attributes.class "btn-outline-secondary"
                                                        , Html.Attributes.class "bi-x-lg"
                                                        , Html.Events.onClick <| UpdateOutputs <| List.removeAt output programme.outputs
                                                        ]
                                                        []
                                                    ]
                                           )
                                    )
                                ]

                            Templates ->
                                [ Html.div
                                    [ Html.Attributes.class "card-header d-flex flex-row align-items-center" ]
                                    [ Html.text "Templates" ]
                                , Html.ul
                                    [ Html.Attributes.class "list-group"
                                    , Html.Attributes.class "list-group-flush"
                                    , Html.Attributes.style "overflow-y" "scroll"
                                    ]
                                    [ Html.li
                                        [ Html.Attributes.class "list-group-item"
                                        ]
                                        [ Html.text "TODO" ]
                                    ]
                                ]

                            Errors ->
                                [ Html.div
                                    [ Html.Attributes.class "card-header d-flex flex-row align-items-center" ]
                                    [ Html.text "Errors" ]
                                , Html.ul
                                    [ Html.Attributes.class "list-group"
                                    , Html.Attributes.class "list-group-flush"
                                    , Html.Attributes.style "overflow-y" "scroll"
                                    ]
                                    (List.indexedMap
                                        (\index error ->
                                            Html.li
                                                [ Html.Attributes.class "list-group-item"
                                                , Html.Attributes.style "display" "flex"
                                                , Html.Attributes.style "flex-direction" "row"
                                                , Html.Attributes.style "align-items" "center"
                                                , Html.Attributes.style "white-space" "pre-wrap"
                                                ]
                                                [ Html.div
                                                    [ Html.Attributes.style "flex" "1"
                                                    , Html.Attributes.style "white-space" "pre-wrap"
                                                    ]
                                                    [ Html.text error ]
                                                , Html.div
                                                    [ Html.Attributes.class "btn"
                                                    , Html.Attributes.class "btn-outline-secondary"
                                                    , Html.Attributes.class "bi-x-lg"
                                                    , Html.Events.onClick <| RemoveErr index
                                                    ]
                                                    []
                                                ]
                                        )
                                        errors
                                    )
                                ]
                       )
                )
            , Html.div
                [ Html.Attributes.class "card"
                , Html.Attributes.style "margin" "3px"
                , Html.Attributes.style "flex" "1"
                ]
              <|
                List.concat
                    [ case ( previewSelected, List.filter (hasId previewSelected) programme.decks ) of
                        ( Just previewSelected_, [ deck ] ) ->
                            let
                                maybeVariant : Maybe Deck.Variant
                                maybeVariant =
                                    List.getAt previewSelected_.variant deck.variants
                            in
                            Html.div
                                [ Html.Attributes.class "card-header"
                                , Html.Attributes.class "d-flex"
                                , Html.Attributes.class "flex-row"
                                , Html.Attributes.class "align-items-center"
                                ]
                                [ Html.text "Preview: "
                                , case previewTab of
                                    Edit _ ->
                                        Html.div
                                            [ Html.Attributes.style "margin-left" "8px" ]
                                            [ Html.input
                                                [ Html.Attributes.class "form-control"
                                                , Html.Attributes.value <| Maybe.withDefault "" deck.title
                                                , Html.Attributes.placeholder <| Deck.getTitle deck
                                                , Html.Events.onInput <| UpdateDeckTitle deck.id
                                                ]
                                                []
                                            ]

                                    _ ->
                                        Html.text <| Deck.getTitle deck
                                ]
                                :: tabBar
                                    [ { msg = PreviewSwitchTab Preview
                                      , label = Html.text "Preview"
                                      , active = previewTab == Preview
                                      }
                                    , { msg =
                                            PreviewSwitchTab <|
                                                Edit
                                                    { list =
                                                        EditableList.init
                                                            { wrapMsg = EditChunkList
                                                            , batchMsgs = Batch
                                                            , listReordered =
                                                                \chunks ->
                                                                    case maybeVariant of
                                                                        Just variant ->
                                                                            UpdateVariant deck.id previewSelected_.variant { variant | chunks = chunks }

                                                                        Nothing ->
                                                                            Batch []
                                                            }
                                                    }
                                      , label = Html.text "Edit"
                                      , active =
                                            case previewTab of
                                                Edit {} ->
                                                    True

                                                _ ->
                                                    False
                                      }
                                    , { msg = PreviewSwitchTab <| Fetch <| Search []
                                      , label = Html.text "Fetch"
                                      , active =
                                            case previewTab of
                                                Fetch _ ->
                                                    True

                                                _ ->
                                                    False
                                      }
                                    , { msg = PreviewSwitchTab (Style { output = 0 })
                                      , label = Html.text "Style"
                                      , active =
                                            case previewTab of
                                                Style {} ->
                                                    True

                                                _ ->
                                                    False
                                      }
                                    ]
                                :: Html.div
                                    [ Html.Attributes.style "display" "flex"
                                    , Html.Attributes.style "flex-direction" "row"
                                    , Html.Attributes.style "align-items" "center"
                                    ]
                                    [ Html.div
                                        [ Html.Attributes.style "flex" "1"
                                        , Html.Attributes.style "width" "0"
                                        ]
                                        [ let
                                            selectVariant index =
                                                Batch
                                                    [ PreviewSelect <| Just { previewSelected_ | variant = index }
                                                    , UpdateDefaultVariant deck.id index
                                                    ]
                                          in
                                          tabBar <|
                                            List.indexedMap
                                                (\index ({ name } as variant) ->
                                                    { msg = selectVariant index
                                                    , label =
                                                        case previewTab of
                                                            Edit _ ->
                                                                Html.div
                                                                    [ Html.Attributes.style "display" "flex"
                                                                    , Html.Attributes.style "align-items" "center"
                                                                    ]
                                                                    [ Html.input
                                                                        [ Html.Attributes.value name
                                                                        , Html.Events.onInput <| UpdateVariantName deck.id index
                                                                        ]
                                                                        []
                                                                    , Html.div
                                                                        [ Html.Attributes.class "bi-x"
                                                                        , Html.Events.stopPropagationOn "click" <| Json.Decode.succeed ( RemoveVariant deck.id index, True )
                                                                        ]
                                                                        []
                                                                    ]

                                                            _ ->
                                                                Html.text name
                                                    , active = previewSelected_.variant == index
                                                    }
                                                )
                                                deck.variants
                                                ++ [ { msg =
                                                        Batch
                                                            [ AddVariant deck.id <|
                                                                Maybe.withDefault { name = "New Variant", chunks = [], style = Dict.empty, slides = Dict.empty } <|
                                                                    maybeVariant
                                                            , selectVariant <| List.length deck.variants
                                                            ]
                                                     , label = Html.text "+"
                                                     , active = False
                                                     }
                                                   ]
                                        ]
                                    , Html.div
                                        [ Html.Attributes.class "btn"
                                        , Html.Attributes.class "btn-outline-primary"
                                        , Html.Attributes.class "bi-floppy"
                                        , Html.Events.onClick <|
                                            RunCmd <|
                                                Http.request
                                                    { method = "PUT"
                                                    , headers = []
                                                    , url = Url.Builder.absolute [ "deck" ] []
                                                    , body =
                                                        Http.jsonBody <|
                                                            Json.Encode.object
                                                                [ ( "title", Json.Encode.string <| Deck.getTitle deck )
                                                                , ( "body"
                                                                  , Json.Encode.string <|
                                                                        String.join "\n" <|
                                                                            List.map
                                                                                (String.join "\n"
                                                                                    << List.map
                                                                                        (\chunk ->
                                                                                            case chunk of
                                                                                                Deck.Title { title, subtitle } ->
                                                                                                    title ++ "\n" ++ subtitle

                                                                                                Deck.Body body ->
                                                                                                    body
                                                                                        )
                                                                                    << .chunks
                                                                                )
                                                                                deck.variants
                                                                  )
                                                                , ( "json", Json.Encode.string <| Json.Encode.encode 0 <| Deck.encodeWithoutId deck )
                                                                ]
                                                    , expect = Http.expectWhatever <| Result.unpack (ShowErr << errorToString) (\() -> Batch [])
                                                    , timeout = Nothing
                                                    , tracker = Nothing
                                                    }
                                        ]
                                        []
                                    ]
                                :: (case maybeVariant of
                                        Nothing ->
                                            []

                                        Just variant ->
                                            [ Html.div
                                                [ Html.Attributes.style "flex" "1"
                                                , Html.Attributes.style "display" "flex"
                                                , Html.Attributes.style "flex-direction" "row"
                                                , Html.Attributes.style "align-items" "stretch"
                                                , Html.Attributes.style "height" "0"
                                                ]
                                                [ Html.div
                                                    [ Html.Attributes.style "flex" "1"
                                                    , Html.Attributes.style "overflow-y" "scroll"
                                                    ]
                                                    [ case previewTab of
                                                        Preview ->
                                                            slidesPanel previewSlide PreviewShowSlide <| Slide.collate variant.slides

                                                        Edit { list } ->
                                                            Html.div []
                                                                [ EditableList.view list
                                                                    "ul"
                                                                    [ Html.Attributes.class "list-group"
                                                                    , Html.Attributes.class "list-group-flush"
                                                                    ]
                                                                  <|
                                                                    List.indexedMap
                                                                        (\chunkIndex chunk ->
                                                                            { id = "deckChunk" ++ String.fromInt chunkIndex
                                                                            , value = chunk
                                                                            , view =
                                                                                \{ topLevelAttributes, dragHandleAttributes } ->
                                                                                    Html.li ([ Html.Attributes.class "list-group-item" ] ++ topLevelAttributes)
                                                                                        (Html.div
                                                                                            [ Html.Attributes.style "display" "flex"
                                                                                            , Html.Attributes.style "flex-direction" "row"
                                                                                            , Html.Attributes.style "align-items" "center"
                                                                                            ]
                                                                                            [ Html.div [ Html.Attributes.style "flex" "1" ]
                                                                                                [ tabBar
                                                                                                    [ let
                                                                                                        active =
                                                                                                            case chunk of
                                                                                                                Deck.Title _ ->
                                                                                                                    True

                                                                                                                _ ->
                                                                                                                    False
                                                                                                      in
                                                                                                      { msg =
                                                                                                            UpdateChunk deck.id previewSelected_.variant chunkIndex <|
                                                                                                                if active then
                                                                                                                    chunk

                                                                                                                else
                                                                                                                    Deck.Title { title = "", subtitle = "" }
                                                                                                      , label = Html.text "Title"
                                                                                                      , active = active
                                                                                                      }
                                                                                                    , let
                                                                                                        active =
                                                                                                            case chunk of
                                                                                                                Deck.Body _ ->
                                                                                                                    True

                                                                                                                _ ->
                                                                                                                    False
                                                                                                      in
                                                                                                      { msg =
                                                                                                            UpdateChunk deck.id previewSelected_.variant chunkIndex <|
                                                                                                                if active then
                                                                                                                    chunk

                                                                                                                else
                                                                                                                    Deck.Body ""
                                                                                                      , label = Html.text "Body"
                                                                                                      , active = active
                                                                                                      }
                                                                                                    ]
                                                                                                ]
                                                                                            , Html.div
                                                                                                ([ Html.Attributes.class "btn"
                                                                                                 , Html.Attributes.class "btn-outline-secondary"
                                                                                                 , Html.Attributes.class "bi-arrow-down-up"
                                                                                                 , Html.Attributes.style "border-top-right-radius" "0px"
                                                                                                 , Html.Attributes.style "border-bottom-right-radius" "0px"
                                                                                                 ]
                                                                                                    ++ dragHandleAttributes
                                                                                                )
                                                                                                []
                                                                                            , Html.div
                                                                                                [ Html.Attributes.class "btn"
                                                                                                , Html.Attributes.class "btn-outline-danger"
                                                                                                , Html.Attributes.class "bi-x-lg"
                                                                                                , Html.Attributes.style "border-top-left-radius" "0px"
                                                                                                , Html.Attributes.style "border-bottom-left-radius" "0px"
                                                                                                , Html.Events.onClick <| RemoveChunk deck.id previewSelected_.variant chunkIndex
                                                                                                ]
                                                                                                []
                                                                                            ]
                                                                                            :: (case chunk of
                                                                                                    Deck.Title { title, subtitle } ->
                                                                                                        [ Html.div []
                                                                                                            [ Html.input
                                                                                                                [ Html.Attributes.style "width" "100%"
                                                                                                                , Html.Attributes.value title
                                                                                                                , Html.Events.onInput
                                                                                                                    (\newTitle ->
                                                                                                                        UpdateChunk deck.id previewSelected_.variant chunkIndex <|
                                                                                                                            Deck.Title { title = newTitle, subtitle = subtitle }
                                                                                                                    )
                                                                                                                ]
                                                                                                                []
                                                                                                            ]
                                                                                                        , Html.div []
                                                                                                            [ Html.input
                                                                                                                [ Html.Attributes.style "width" "100%"
                                                                                                                , Html.Attributes.value subtitle
                                                                                                                , Html.Events.onInput
                                                                                                                    (\newSubtitle ->
                                                                                                                        UpdateChunk deck.id previewSelected_.variant chunkIndex <|
                                                                                                                            Deck.Title { title = title, subtitle = newSubtitle }
                                                                                                                    )
                                                                                                                ]
                                                                                                                []
                                                                                                            ]
                                                                                                        ]

                                                                                                    Deck.Body body ->
                                                                                                        [ Html.div
                                                                                                            [ Html.Attributes.style "display" "flex"
                                                                                                            , Html.Attributes.style "align-items" "stretch"
                                                                                                            ]
                                                                                                            [ Html.textarea
                                                                                                                [ Html.Attributes.style "width" "100%"
                                                                                                                , Html.Attributes.style "flex-shrink" "0"
                                                                                                                , Html.Attributes.style "padding" "2px"
                                                                                                                , Html.Attributes.style "resize" "none"
                                                                                                                , Html.Events.onInput <|
                                                                                                                    UpdateChunk deck.id previewSelected_.variant chunkIndex
                                                                                                                        << Deck.Body
                                                                                                                , Html.Events.preventDefaultOn "keydown" <|
                                                                                                                    Json.Decode.map5
                                                                                                                        (\key alt selectionStart selectionEnd value ->
                                                                                                                            case ( key, alt ) of
                                                                                                                                ( "Enter", True ) ->
                                                                                                                                    ( UpdateVariant deck.id
                                                                                                                                        previewSelected_.variant
                                                                                                                                        { variant
                                                                                                                                            | chunks =
                                                                                                                                                List.take chunkIndex variant.chunks
                                                                                                                                                    ++ [ Deck.Body <| String.trim <| String.left selectionStart value
                                                                                                                                                       , Deck.Body <| String.trim <| String.dropLeft selectionEnd value
                                                                                                                                                       ]
                                                                                                                                                    ++ List.drop (chunkIndex + 1) variant.chunks
                                                                                                                                        }
                                                                                                                                    , True
                                                                                                                                    )

                                                                                                                                ( "Backspace", True ) ->
                                                                                                                                    ( RemoveChunk deck.id previewSelected_.variant chunkIndex
                                                                                                                                    , True
                                                                                                                                    )

                                                                                                                                _ ->
                                                                                                                                    ( Batch [], False )
                                                                                                                        )
                                                                                                                        (Json.Decode.field "key" Json.Decode.string)
                                                                                                                        (Json.Decode.field "altKey" Json.Decode.bool)
                                                                                                                        (Json.Decode.at [ "target", "selectionStart" ] Json.Decode.int)
                                                                                                                        (Json.Decode.at [ "target", "selectionEnd" ] Json.Decode.int)
                                                                                                                        Html.Events.targetValue
                                                                                                                , Html.Attributes.value body
                                                                                                                , Html.Attributes.rows (body |> String.lines |> List.length)
                                                                                                                ]
                                                                                                                []
                                                                                                            , Html.div
                                                                                                                [ Html.Attributes.style "visibility" "hidden"
                                                                                                                , Html.Attributes.style "width" "100%"
                                                                                                                , Html.Attributes.style "flex-shrink" "0"
                                                                                                                , Html.Attributes.style "padding" "2px"
                                                                                                                , Html.Attributes.style "white-space" "pre-wrap"
                                                                                                                ]
                                                                                                                [ Html.text body ]
                                                                                                            ]
                                                                                                        ]
                                                                                               )
                                                                                        )
                                                                            }
                                                                        )
                                                                    <|
                                                                        variant.chunks
                                                                , Html.div
                                                                    [ Html.Attributes.class "btn"
                                                                    , Html.Attributes.class "btn-outline-secondary"
                                                                    , Html.Attributes.class "bi-plus-lg"
                                                                    , Html.Attributes.style "width" "100%"
                                                                    , Html.Attributes.style "border-top-left-radius" "0px"
                                                                    , Html.Attributes.style "border-top-right-radius" "0px"
                                                                    , Html.Attributes.style "border-top" "none"
                                                                    , Html.Attributes.style "border-left" "none"
                                                                    , Html.Attributes.style "border-right" "none"
                                                                    , Html.Events.onClick <| AddChunk deck.id previewSelected_.variant <| Deck.Body ""
                                                                    ]
                                                                    []
                                                                ]

                                                        Fetch fetchTab ->
                                                            Html.div []
                                                                [ tabBar
                                                                    [ { msg = PreviewSwitchTab <| Fetch <| Search []
                                                                      , label = Html.text "Search"
                                                                      , active =
                                                                            case fetchTab of
                                                                                Search _ ->
                                                                                    True

                                                                                _ ->
                                                                                    False
                                                                      }
                                                                    , { msg = PreviewSwitchTab <| Fetch <| Bible Fetch.Bible.default
                                                                      , label = Html.text "Bible"
                                                                      , active =
                                                                            case fetchTab of
                                                                                Bible _ ->
                                                                                    True

                                                                                _ ->
                                                                                    False
                                                                      }
                                                                    , { msg = PreviewSwitchTab <| Fetch <| Psalm Fetch.Psalm.default
                                                                      , label = Html.text "Psalm"
                                                                      , active =
                                                                            case fetchTab of
                                                                                Psalm _ ->
                                                                                    True

                                                                                _ ->
                                                                                    False
                                                                      }
                                                                    , { msg = PreviewSwitchTab <| Fetch <| Hymn Fetch.Hymn.default
                                                                      , label = Html.text "Hymn"
                                                                      , active =
                                                                            case fetchTab of
                                                                                Hymn _ ->
                                                                                    True

                                                                                _ ->
                                                                                    False
                                                                      }
                                                                    ]
                                                                , case fetchTab of
                                                                    Search results ->
                                                                        Html.div []
                                                                            [ Html.div
                                                                                [ Html.Attributes.style "margin" "8px"
                                                                                ]
                                                                                [ Html.input
                                                                                    [ Html.Attributes.class "form-control"
                                                                                    , Html.Events.onInput <|
                                                                                        \query ->
                                                                                            RunCmd <|
                                                                                                Http.get
                                                                                                    { url = Url.Builder.absolute [ "search" ] [ Url.Builder.string "query" query ]
                                                                                                    , expect =
                                                                                                        Http.expectJson
                                                                                                            (Result.unpack (ShowErr << errorToString) (PreviewSwitchTab << Fetch << Search))
                                                                                                            (Json.Decode.list Deck.decodeWithoutIdOrSlides)
                                                                                                    }
                                                                                    ]
                                                                                    []
                                                                                ]
                                                                            , Html.ul
                                                                                [ Html.Attributes.class "list-group"
                                                                                , Html.Attributes.style "margin" "8px"
                                                                                , Html.Attributes.style "overflow-y" "scroll"
                                                                                ]
                                                                              <|
                                                                                List.indexedMap
                                                                                    (\index f ->
                                                                                        let
                                                                                            result =
                                                                                                f deck.id
                                                                                        in
                                                                                        Html.div
                                                                                            [ Html.Attributes.class "list-group-item"
                                                                                            , Html.Attributes.classList [ ( "active", deck == result ) ]
                                                                                            , Html.Attributes.style "display" "flex"
                                                                                            , Html.Attributes.style "flex-direction" "row"
                                                                                            , Html.Attributes.style "align-items" "center"
                                                                                            , Html.Events.onClick <| UpdateDeck deck.id result
                                                                                            ]
                                                                                            [ Html.div
                                                                                                [ Html.Attributes.style "flex" "1" ]
                                                                                                [ Html.text <| Deck.getTitle result ]
                                                                                            , Html.div
                                                                                                [ Html.Attributes.class "btn"
                                                                                                , Html.Attributes.class "btn-outline-secondary"
                                                                                                , Html.Attributes.class "bi-x"
                                                                                                , Html.Attributes.style "padding" "0"
                                                                                                , Html.Events.onClick <|
                                                                                                    RunCmd <|
                                                                                                        Http.request
                                                                                                            { method = "DELETE"
                                                                                                            , headers = []
                                                                                                            , url = Url.Builder.absolute [ "deck" ] [ Url.Builder.string "title" <| Deck.getTitle deck ]
                                                                                                            , body = Http.emptyBody
                                                                                                            , expect =
                                                                                                                Http.expectWhatever <|
                                                                                                                    Result.unpack
                                                                                                                        (ShowErr << errorToString)
                                                                                                                        (\() -> PreviewSwitchTab <| Fetch <| Search <| List.removeAt index results)
                                                                                                            , timeout = Nothing
                                                                                                            , tracker = Nothing
                                                                                                            }
                                                                                                ]
                                                                                                []
                                                                                            ]
                                                                                    )
                                                                                    results
                                                                            ]

                                                                    Bible params ->
                                                                        Html.div []
                                                                            [ Form.input params (PreviewSwitchTab << Fetch << Bible) <|
                                                                                Form.fieldList
                                                                                    [ Form.map ( .version, \f p -> { p | version = f p.version } ) <| Form.field "Version"
                                                                                    , Form.map ( .reference, \f p -> { p | reference = f p.reference } ) <| Form.field "Reference"
                                                                                    , Form.map ( .framing, \f p -> { p | framing = f p.framing } ) <|
                                                                                        Form.fieldSelect "Framing"
                                                                                            [ { label = "None", default = Fetch.Bible.None, isThis = (==) Fetch.Bible.None }
                                                                                            , { label = "Standard", default = Fetch.Bible.Standard, isThis = (==) Fetch.Bible.Standard }
                                                                                            , { label = "Gospel", default = Fetch.Bible.Gospel, isThis = (==) Fetch.Bible.Gospel }
                                                                                            , { label = "Lent Gospel", default = Fetch.Bible.LentGospel, isThis = (==) Fetch.Bible.LentGospel }
                                                                                            ]
                                                                                    ]
                                                                            , Html.div
                                                                                [ Html.Attributes.class "btn"
                                                                                , Html.Attributes.class "btn-outline-primary"
                                                                                , Html.Attributes.style "width" "100%"
                                                                                , Html.Attributes.style "margin-top" "4px"
                                                                                , Html.Events.onClick <|
                                                                                    RunCmd <|
                                                                                        Task.attempt
                                                                                            (\res ->
                                                                                                case res of
                                                                                                    Err e ->
                                                                                                        ShowErr e

                                                                                                    Ok chunks ->
                                                                                                        UpdateVariant deck.id previewSelected_.variant { variant | chunks = chunks }
                                                                                            )
                                                                                        <|
                                                                                            Fetch.Bible.fetch params
                                                                                ]
                                                                                [ Html.text "Fetch" ]
                                                                            ]

                                                                    Psalm params ->
                                                                        Html.div []
                                                                            [ Form.input params (PreviewSwitchTab << Fetch << Psalm) <|
                                                                                Form.fieldList
                                                                                    [ Form.map ( .psalter, \f p -> { p | psalter = f p.psalter } ) <|
                                                                                        Form.fieldSelect "Psalter"
                                                                                            [ { label = "BCP", default = Fetch.Psalm.BCP, isThis = (==) Fetch.Psalm.BCP }
                                                                                            , { label = "CW", default = Fetch.Psalm.CW, isThis = (==) Fetch.Psalm.CW }
                                                                                            ]
                                                                                    , Form.map ( .number, \f p -> { p | number = f p.number } ) <| Form.map Form.int <| Form.field "Number"
                                                                                    , Form.map ( .startVerse, \f p -> { p | startVerse = f p.startVerse } ) <| Form.map Form.int <| Form.field "Start Verse"
                                                                                    , Form.map ( .endVerse, \f p -> { p | endVerse = f p.endVerse } ) <| Form.map Form.int <| Form.field "End Verse"
                                                                                    , Form.map ( .gloria, \f p -> { p | gloria = f p.gloria } ) <|
                                                                                        Form.fieldSelect "Gloria"
                                                                                            [ { label = "True", default = True, isThis = identity }
                                                                                            , { label = "False", default = False, isThis = not }
                                                                                            ]
                                                                                    , Form.map ( .bold, \f p -> { p | bold = f p.bold } ) <|
                                                                                        Form.fieldSelect "Bold"
                                                                                            [ { label = "None", default = Fetch.Psalm.None, isThis = (==) Fetch.Psalm.None }
                                                                                            , { label = "Even Verses", default = Fetch.Psalm.EvenVerses, isThis = (==) Fetch.Psalm.EvenVerses }
                                                                                            , { label = "Second Half", default = Fetch.Psalm.SecondHalf, isThis = (==) Fetch.Psalm.SecondHalf }
                                                                                            ]
                                                                                    ]
                                                                            , Html.div
                                                                                [ Html.Attributes.class "btn"
                                                                                , Html.Attributes.class "btn-outline-primary"
                                                                                , Html.Attributes.style "width" "100%"
                                                                                , Html.Attributes.style "margin-top" "4px"
                                                                                , Html.Events.onClick <|
                                                                                    RunCmd <|
                                                                                        Task.attempt
                                                                                            (\res ->
                                                                                                case res of
                                                                                                    Err e ->
                                                                                                        ShowErr e

                                                                                                    Ok chunks ->
                                                                                                        UpdateVariant deck.id previewSelected_.variant { variant | chunks = chunks }
                                                                                            )
                                                                                        <|
                                                                                            Fetch.Psalm.fetch params
                                                                                ]
                                                                                [ Html.text "Fetch" ]
                                                                            ]

                                                                    Hymn params ->
                                                                        Html.div []
                                                                            [ Form.input params (PreviewSwitchTab << Fetch << Hymn) <|
                                                                                Form.fieldList
                                                                                    [ Form.map ( .hymnal, \f p -> { p | hymnal = f p.hymnal } ) <|
                                                                                        Form.fieldSelect "Hymnal"
                                                                                            [ { label = "NEH", default = Fetch.Hymn.NEH, isThis = (==) Fetch.Hymn.NEH }
                                                                                            , { label = "A&M", default = Fetch.Hymn.AM, isThis = (==) Fetch.Hymn.AM }
                                                                                            ]
                                                                                    , Form.map ( .number, \f p -> { p | number = f p.number } ) <| Form.map Form.int <| Form.field "Number"
                                                                                    ]
                                                                            , Html.div
                                                                                [ Html.Attributes.class "btn"
                                                                                , Html.Attributes.class "btn-outline-primary"
                                                                                , Html.Attributes.style "width" "100%"
                                                                                , Html.Attributes.style "margin-top" "4px"
                                                                                , Html.Events.onClick <|
                                                                                    RunCmd <|
                                                                                        Task.attempt
                                                                                            (\res ->
                                                                                                case res of
                                                                                                    Err e ->
                                                                                                        ShowErr e

                                                                                                    Ok chunks ->
                                                                                                        UpdateVariant deck.id previewSelected_.variant { variant | chunks = chunks }
                                                                                            )
                                                                                        <|
                                                                                            Fetch.Hymn.fetch params
                                                                                ]
                                                                                [ Html.text "Fetch" ]
                                                                            ]
                                                                ]

                                                        Style { output } ->
                                                            Html.div []
                                                                [ tabBar
                                                                    (List.indexedMap
                                                                        (\index { name } -> { msg = PreviewSwitchTab <| Style { output = index }, label = Html.text name, active = output == index })
                                                                        programme.outputs
                                                                    )
                                                                , case List.getAt output programme.outputs of
                                                                    Nothing ->
                                                                        Html.text "Error"

                                                                    Just ({ name } as output_) ->
                                                                        StylePanel.localStylePanel
                                                                            { default = output_.style
                                                                            , style = Maybe.withDefault Style.emptyLocal (Dict.get name variant.style)
                                                                            , updateStyle = UpdateVariantStyle deck.id previewSelected_.variant name
                                                                            }
                                                                ]
                                                    ]
                                                , Html.div
                                                    [ Html.Attributes.class "text-bg-danger"
                                                    , Html.Attributes.class "bi-chevron-right"
                                                    , Html.Attributes.style "display" "flex"
                                                    , Html.Attributes.style "justify-content" "center"
                                                    , Html.Attributes.style "align-items" "center"
                                                    , Html.Attributes.style "padding" "2px"
                                                    , Html.Events.onClick <|
                                                        Batch
                                                            [ LiveSend <|
                                                                Just
                                                                    { title = Deck.getTitle deck
                                                                    , slide = previewSlide
                                                                    , slides = Slide.collate variant.slides
                                                                    }
                                                            , case
                                                                programme.decks
                                                                    |> List.dropWhile ((/=) deck)
                                                                    |> List.tail
                                                                    |> Maybe.andThen List.head
                                                              of
                                                                Just nextDeck ->
                                                                    PreviewSelect (Just { id = nextDeck.id, variant = nextDeck.defaultVariant })

                                                                Nothing ->
                                                                    Batch []
                                                            ]
                                                    ]
                                                    []
                                                ]
                                            ]
                                   )

                        _ ->
                            [ Html.div
                                [ Html.Attributes.class "card-header"
                                , Html.Attributes.class "d-flex"
                                , Html.Attributes.class "flex-row"
                                , Html.Attributes.class "align-items-center"
                                ]
                                [ Html.text "Preview" ]
                            , Html.div
                                [ Html.Attributes.style "flex" "1"
                                , Html.Attributes.style "display" "flex"
                                , Html.Attributes.style "justify-content" "center"
                                , Html.Attributes.style "align-items" "center"
                                ]
                                [ Html.text "Nothing selected" ]
                            ]
                    , List.map (Html.map PreviewPreviewMsg) <|
                        previewPanel previewPreview programme.outputs <|
                            Output.init <|
                                Maybe.andThen
                                    (\previewSelected_ ->
                                        case List.filter (hasId previewSelected) programme.decks of
                                            [ deck ] ->
                                                deck.variants
                                                    |> List.getAt previewSelected_.variant
                                                    |> Maybe.andThen
                                                        (\variant ->
                                                            variant.slides
                                                                |> Slide.collate
                                                                |> List.getAt previewSlide
                                                                |> Maybe.andThen (\( _, slide ) -> Dict.get previewPreview.output slide)
                                                        )

                                            _ ->
                                                Nothing
                                    )
                                <|
                                    previewSelected
                    ]
            , Html.div
                [ Html.Attributes.class "card"
                , Html.Attributes.style "margin" "3px"
                , Html.Attributes.style "flex" "1"
                ]
              <|
                List.concat
                    [ [ Html.div
                            [ Html.Attributes.class "card-header d-flex flex-row align-items-center" ]
                            [ Html.div
                                [ Html.Attributes.style "flex" "1" ]
                                [ Html.text <|
                                    case live of
                                        Just { title } ->
                                            "Live: " ++ title

                                        Nothing ->
                                            "Live"
                                ]
                            , Html.div
                                [ Html.Attributes.class "btn"
                                , Html.Attributes.class "btn-outline-danger"
                                , Html.Attributes.class "bi-x-lg"
                                , Html.Events.onClick <| LiveSend Nothing
                                ]
                                []
                            ]
                      , case live of
                            Nothing ->
                                Html.div
                                    [ Html.Attributes.style "flex" "1"
                                    , Html.Attributes.style "display" "flex"
                                    , Html.Attributes.style "justify-content" "center"
                                    , Html.Attributes.style "align-items" "center"
                                    ]
                                    [ Html.text "Nothing selected" ]

                            Just ({ slide, slides } as live_) ->
                                Html.div
                                    [ Html.Attributes.style "flex" "1"
                                    , Html.Attributes.style "overflow-y" "scroll"
                                    ]
                                    [ slidesPanel slide (\slide_ -> LiveSend <| Just { live_ | slide = slide_ }) slides ]
                      ]
                    , List.map (Html.map LivePreviewMsg) <|
                        previewPanel livePreview programme.outputs liveOutput
                    ]
            ]
        ]
    }


hasId : Maybe { a | id : Int } -> { b | id : Int } -> Bool
hasId x y =
    case x of
        Just x2 ->
            x2.id == y.id

        Nothing ->
            False


slidesPanel : Int -> (Int -> msg) -> List ( String, Dict String Slide ) -> Html msg
slidesPanel activeIndex showSlide slides =
    Html.ul
        [ Html.Attributes.class "list-group"
        , Html.Attributes.class "list-group-flush"
        ]
        (slides
            |> List.indexedMap
                (\index ( text, _ ) ->
                    Html.li
                        [ Html.Attributes.class "list-group-item"
                        , Html.Attributes.classList [ ( "active", index == activeIndex ) ]
                        , Html.Events.onClick (showSlide index)
                        ]
                        (List.map (\line -> Html.p [ Html.Attributes.style "margin" "0px" ] [ Html.text line ])
                            (String.lines text)
                        )
                )
        )


previewPanel : PreviewPanelModel -> List Programme.Output -> Output.Model a -> List (Html PreviewPanelMsg)
previewPanel { height, output, elementId } outputs model =
    [ Html.div
        (List.concat
            [ [ Html.Attributes.style "background" "var(--bs-secondary-bg)"
              , Html.Attributes.style "height" "4px"
              ]
            , case getOutput output outputs of
                Just _ ->
                    Html.Attributes.style "cursor" "row-resize"
                        :: Draggable.mouseTrigger () PreviewPanelDragMsg
                        :: Draggable.touchTriggers () PreviewPanelDragMsg

                Nothing ->
                    []
            ]
        )
        []
    , tabBar
        (List.map
            (\{ name } ->
                { msg = PreviewPanelSelectOutput name
                , label = Html.text name
                , active = output == name
                }
            )
            outputs
        )
    , case getOutput output outputs of
        Nothing ->
            Html.div
                [ Html.Attributes.style "height" "0"
                , Html.Attributes.id elementId
                ]
                [ Html.text "" ]

        Just output_ ->
            Html.div
                [ Html.Attributes.style "height" (String.fromFloat height ++ "px")
                , Html.Attributes.id elementId
                ]
                [ let
                    size =
                        output_.style.size
                  in
                  Html.div
                    [ Html.Attributes.style "width" (String.fromFloat (height * toFloat size.width / toFloat size.height))
                    , Html.Attributes.style "height" (String.fromFloat height)
                    , Html.Attributes.style "margin" "auto"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "width" (String.fromInt size.width)
                        , Html.Attributes.style "height" (String.fromInt size.height)
                        , Html.Attributes.style "scale" (String.fromFloat (height / toFloat size.height))
                        , Html.Attributes.style "transform-origin" "top left"
                        ]
                        [ Output.view model ]
                    ]
                ]
    ]


tabBar : List { msg : msg, label : Html msg, active : Bool } -> Html msg
tabBar tabs =
    Html.div
        [ Html.Attributes.style "overflow-x" "scroll"
        , Html.Attributes.style "overflow-y" "hidden"
        , Html.Attributes.style "flex" "0 0 auto"
        ]
        [ Html.ul
            [ Html.Attributes.class "nav"
            , Html.Attributes.class "nav-tabs"
            , Html.Attributes.style "padding" "1px 2px 0px"
            , Html.Attributes.style "flex-wrap" "nowrap"
            ]
            (List.map
                (\{ msg, label, active } ->
                    Html.li [ Html.Attributes.class "nav-item" ]
                        [ Html.a
                            [ Html.Attributes.class "nav-link"
                            , Html.Attributes.classList [ ( "active", active ) ]
                            , Html.Attributes.style "padding" "4px 8px"
                            , Html.Events.onClick msg
                            ]
                            [ label ]
                        ]
                )
                tabs
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        programmeSend_ : Programme -> List ( Deck.Chunk, Style ) -> ( Model, Cmd Msg )
        programmeSend_ programme chunks =
            ( { model | programme = programme }
            , Cmd.batch
                [ programmeSend (Programme.encode programme)
                , splitChunks chunks
                ]
            )

        splitChunks : List ( Deck.Chunk, Style ) -> Cmd Msg
        splitChunks chunks =
            Cmd.batch <|
                List.map chunkSplitSend <|
                    List.map
                        (\( chunk, style ) ->
                            Json.Encode.object [ ( "chunk", Deck.encodeChunk chunk ), ( "style", Style.encode style ) ]
                        )
                        chunks

        updateDecks : List Deck -> Programme
        updateDecks decks =
            let
                programme =
                    model.programme
            in
            { programme | decks = decks }

        updateDeck : Int -> (Deck -> Deck) -> Programme
        updateDeck id f =
            updateDecks <|
                List.map
                    (\deck ->
                        if deck.id == id then
                            f deck

                        else
                            deck
                    )
                <|
                    model.programme.decks

        updateVariant : Int -> Int -> (Deck.Variant -> Deck.Variant) -> Programme
        updateVariant deckId variantId f =
            updateDeck deckId <|
                \deck -> { deck | variants = List.updateAt variantId f deck.variants }

        variantChunksAndStyles : List Programme.Output -> Deck.Variant -> List ( Deck.Chunk, Style )
        variantChunksAndStyles outputs variant =
            List.concatMap (getStyles outputs variant.style) variant.chunks

        getStyles : List Programme.Output -> Dict String Style.LocalStyle -> Deck.Chunk -> List ( Deck.Chunk, Style )
        getStyles outputs localStyle chunk =
            outputs
                |> List.map
                    (\output ->
                        ( chunk
                        , Style.addLocal output.style <| Maybe.withDefault Style.emptyLocal <| Dict.get output.name localStyle
                        )
                    )

        updateSlides : ChunkToSlides -> Programme -> Programme
        updateSlides chunkToSlides programme =
            { programme
                | decks =
                    programme.decks
                        |> List.map
                            (\deck ->
                                { deck
                                    | variants =
                                        deck.variants
                                            |> List.map
                                                (\variant ->
                                                    { variant
                                                        | slides =
                                                            Dict.fromList <|
                                                                List.map
                                                                    (\output ->
                                                                        ( output.name
                                                                        , variant.chunks
                                                                            |> List.filterMap
                                                                                (\chunk2 ->
                                                                                    Dict.Any.get
                                                                                        ( chunk2
                                                                                        , Style.addLocal output.style <|
                                                                                            Maybe.withDefault Style.emptyLocal <|
                                                                                                Dict.get output.name variant.style
                                                                                        )
                                                                                        chunkToSlides
                                                                                )
                                                                            |> List.concat
                                                                        )
                                                                    )
                                                                    model.programme.outputs
                                                    }
                                                )
                                }
                            )
            }
    in
    case msg of
        Batch msgs ->
            let
                ( model4, cmds2 ) =
                    List.foldl
                        (\msg2 ( model2, cmds ) ->
                            let
                                ( model3, cmd ) =
                                    update msg2 model2
                            in
                            ( model3, cmd :: cmds )
                        )
                        ( model, [] )
                        msgs
            in
            ( model4, Cmd.batch cmds2 )

        RunCmd cmd ->
            ( model, cmd )

        ChunkSplitRecv ( chunk, style ) slides ->
            let
                chunkToSlides =
                    Dict.Any.insert ( chunk, style ) slides model.chunkToSlides
            in
            ( { model
                | chunkToSlides = chunkToSlides
                , programme = updateSlides chunkToSlides model.programme
              }
            , Cmd.none
            )

        ProgrammeRecv programme ->
            ( { model | programme = updateSlides model.chunkToSlides programme }, Cmd.none )

        RefreshSlides ->
            ( model
            , splitChunks <|
                (model.programme.decks
                    |> List.concatMap .variants
                    |> List.concatMap (variantChunksAndStyles model.programme.outputs)
                )
            )

        UpdateOutputs outputs ->
            let
                programme =
                    model.programme
            in
            programmeSend_
                { programme | outputs = outputs }
                (model.programme.decks
                    |> List.concatMap .variants
                    |> List.concatMap (variantChunksAndStyles outputs)
                )

        UpdateDecks decks ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateDecks decks)
                (decks
                    |> List.concatMap .variants
                    |> List.concatMap (variantChunksAndStyles model.programme.outputs)
                )

        AddDeck deck ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateDecks <| model.programme.decks ++ [ deck ])
                (deck.variants |> List.concatMap (variantChunksAndStyles model.programme.outputs))

        RemoveDeck deckId ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateDecks <| List.filter (\{ id } -> id /= deckId) model.programme.decks)
                []

        UpdateDeck deckId deck ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateDeck deckId <| \_ -> deck)
                (deck.variants |> List.concatMap (variantChunksAndStyles model.programme.outputs))

        UpdateDeckTitle id title ->
            programmeSend_
                (updateSlides model.chunkToSlides <|
                    updateDeck id <|
                        \deck ->
                            { deck
                                | title =
                                    if title == "" then
                                        Nothing

                                    else
                                        Just title
                            }
                )
                []

        AddVariant deckId variant ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateDeck deckId <| \deck -> { deck | variants = deck.variants ++ [ variant ] })
                (variantChunksAndStyles model.programme.outputs variant)

        RemoveVariant deckId variantId ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateDeck deckId <| \deck -> { deck | variants = List.removeAt variantId deck.variants })
                []

        UpdateVariant deckId variantId variant ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateVariant deckId variantId <| \_ -> variant)
                (variantChunksAndStyles model.programme.outputs variant)

        UpdateVariantName deckId variantId name ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateVariant deckId variantId <| \variant -> { variant | name = name })
                []

        UpdateVariantStyle deckId variantId name style ->
            let
                programme =
                    updateSlides model.chunkToSlides <| updateVariant deckId variantId <| \variant -> { variant | style = Dict.insert name style variant.style }
            in
            programmeSend_ programme
                (programme.decks
                    |> List.find (\{ id } -> id == deckId)
                    |> Maybe.map .variants
                    |> Maybe.andThen (List.getAt variantId)
                    |> Maybe.map (variantChunksAndStyles programme.outputs)
                    |> Maybe.withDefault []
                )

        AddChunk deckId variantId chunk ->
            let
                programme =
                    updateSlides model.chunkToSlides <| updateVariant deckId variantId <| \variant -> { variant | chunks = variant.chunks ++ [ chunk ] }
            in
            programmeSend_ programme
                (programme.decks
                    |> List.find (\{ id } -> id == deckId)
                    |> Maybe.map .variants
                    |> Maybe.andThen (List.getAt variantId)
                    |> Maybe.map (\{ style } -> getStyles programme.outputs style chunk)
                    |> Maybe.withDefault []
                )

        RemoveChunk deckId variantId chunkId ->
            programmeSend_
                (updateSlides model.chunkToSlides <| updateVariant deckId variantId <| \variant -> { variant | chunks = List.removeAt chunkId variant.chunks })
                []

        UpdateChunk deckId variantId chunkId chunk ->
            let
                programme =
                    updateSlides model.chunkToSlides <| updateVariant deckId variantId <| \variant -> { variant | chunks = List.setAt chunkId chunk variant.chunks }
            in
            programmeSend_ programme
                (programme.decks
                    |> List.find (\{ id } -> id == deckId)
                    |> Maybe.map .variants
                    |> Maybe.andThen (List.getAt variantId)
                    |> Maybe.map (\{ style } -> getStyles programme.outputs style chunk)
                    |> Maybe.withDefault []
                )

        UpdateDefaultVariant deckId variantId ->
            programmeSend_ (updateDeck deckId <| \deck -> { deck | defaultVariant = variantId }) []

        OnResize ->
            let
                ( previewPreview, previewCmd ) =
                    previewPanelOnResize model.previewPreview PreviewPreviewMsg

                ( livePreview, liveCmd ) =
                    previewPanelOnResize model.livePreview LivePreviewMsg
            in
            ( { model | previewPreview = previewPreview, livePreview = livePreview }
            , Cmd.batch [ previewCmd, liveCmd ]
            )

        ProgrammeSwitchTab tab ->
            ( { model | programmeTab = tab }, Cmd.none )

        ProgrammeList msg_ ->
            let
                ( programmeList, cmd ) =
                    EditableList.update msg_ model.programmeList
            in
            ( { model | programmeList = programmeList }, cmd )

        PreviewShowSlide slide ->
            ( { model | previewSlide = slide }, Cmd.none )

        PreviewSelect previewSelected ->
            ( { model | previewSlide = 0, previewSelected = previewSelected }, Cmd.none )

        PreviewPreviewMsg previewMsg ->
            let
                ( previewPreview, cmd ) =
                    previewPanelUpdate previewMsg model.previewPreview model.programme.outputs PreviewPreviewMsg
            in
            ( { model | previewPreview = previewPreview }, cmd )

        PreviewSwitchTab tab ->
            ( { model | previewTab = tab }, Cmd.none )

        EditChunkList msg_ ->
            case model.previewTab of
                Edit { list } ->
                    let
                        ( list_, cmd ) =
                            EditableList.update msg_ list
                    in
                    ( { model | previewTab = Edit { list = list_ } }, cmd )

                _ ->
                    ( model, Cmd.none )

        LiveRecv live ->
            let
                liveOutput =
                    Output.update
                        (Output.ShowSlide <|
                            Maybe.andThen (Dict.get model.livePreview.output << Tuple.second) <|
                                Maybe.andThen (\{ slide, slides } -> List.getAt slide slides) <|
                                    live
                        )
                        model.liveOutput
            in
            ( { model | live = live, liveOutput = liveOutput }, Cmd.none )

        LiveSend live ->
            ( { model | live = live }, Output.send live )

        LiveOutput outputMsg ->
            ( { model | liveOutput = Output.update outputMsg model.liveOutput }
            , Cmd.none
            )

        LivePreviewMsg previewMsg ->
            let
                ( livePreview, cmd ) =
                    previewPanelUpdate previewMsg model.livePreview model.programme.outputs LivePreviewMsg

                liveOutput =
                    if livePreview.output /= model.livePreview.output then
                        Output.update
                            (Output.ShowSlide <|
                                Maybe.andThen (Dict.get livePreview.output << Tuple.second) <|
                                    Maybe.andThen (\{ slide, slides } -> List.getAt slide slides) <|
                                        model.live
                            )
                            model.liveOutput

                    else
                        model.liveOutput
            in
            ( { model | livePreview = livePreview, liveOutput = liveOutput }, cmd )

        ShowErr error ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        RemoveErr i ->
            ( { model | errors = List.removeAt i model.errors }, Cmd.none )


previewPanelUpdate :
    PreviewPanelMsg
    -> PreviewPanelModel
    -> List Programme.Output
    -> (PreviewPanelMsg -> Msg)
    -> ( PreviewPanelModel, Cmd Msg )
previewPanelUpdate msg model outputs mapMsg =
    case msg of
        PreviewPanelDragBy ( dx, dy ) ->
            ( { model | height = model.height - dy }
                |> updatePreviewPanelHeight outputs
            , Cmd.none
            )

        PreviewPanelDragMsg dragMsg ->
            let
                ( { drag }, cmd ) =
                    Draggable.update
                        (Draggable.basicConfig <| mapMsg << PreviewPanelDragBy)
                        dragMsg
                        { drag = model.dragState }
            in
            ( { model | dragState = drag }, cmd )

        PreviewPanelSelectOutput output ->
            ( { model | output = output }
                |> updatePreviewPanelHeight outputs
            , Cmd.none
            )

        UpdatePreviewPanelSize size ->
            ( { model | availableWidth = size.width }
                |> updatePreviewPanelHeight outputs
            , Cmd.none
            )


updatePreviewPanelHeight : List Programme.Output -> PreviewPanelModel -> PreviewPanelModel
updatePreviewPanelHeight outputs model =
    case getOutput model.output outputs of
        Nothing ->
            model

        Just output ->
            { model
                | height =
                    model.height
                        |> min
                            (model.availableWidth
                                * toFloat output.style.size.height
                                / toFloat output.style.size.width
                            )
                        |> max 0
            }


getOutput : String -> List Programme.Output -> Maybe Programme.Output
getOutput outputName outputs =
    case List.filter ((==) outputName << .name) outputs of
        [ output ] ->
            Just output

        _ ->
            Nothing


previewPanelOnResize : PreviewPanelModel -> (PreviewPanelMsg -> Msg) -> ( PreviewPanelModel, Cmd Msg )
previewPanelOnResize model mapMsg =
    ( model
    , Task.attempt
        (\result ->
            case result of
                Ok { element } ->
                    mapMsg (UpdatePreviewPanelSize { width = element.width, height = element.height })

                Err (Browser.Dom.NotFound id) ->
                    ShowErr ("Preview panel can't find element: " ++ id)
        )
        (Browser.Dom.getElement model.elementId)
    )


subscriptions : Model -> Sub Msg
subscriptions { programmeList, previewTab, previewPreview, liveOutput, livePreview } =
    Sub.batch
        [ programmeRecv <|
            \value ->
                case Json.Decode.decodeValue Programme.decode value of
                    Ok programme ->
                        ProgrammeRecv programme

                    Err err ->
                        ShowErr (Json.Decode.errorToString err)
        , chunkSplitRecv <|
            \value ->
                case
                    Json.Decode.decodeValue
                        (Json.Decode.map3 (\chunk style slides -> ( chunk, style, slides ))
                            (Json.Decode.field "chunk" Deck.decodeChunk)
                            (Json.Decode.field "style" Style.decode)
                            (Json.Decode.field "slides" <| Json.Decode.list Slide.decode)
                        )
                        value
                of
                    Ok ( chunk, style, slides ) ->
                        ChunkSplitRecv ( chunk, style ) slides

                    Err err ->
                        ShowErr (Json.Decode.errorToString err)
        , Browser.Events.onResize <| \_ _ -> OnResize
        , EditableList.subscriptions programmeList
        , case previewTab of
            Edit { list } ->
                EditableList.subscriptions list

            _ ->
                Sub.none
        , Output.liveSubscriptions LiveRecv
        , Sub.map LiveOutput <| Output.subscriptions liveOutput
        , Sub.map PreviewPreviewMsg <| previewPanelSubscriptions previewPreview
        , Sub.map LivePreviewMsg <| previewPanelSubscriptions livePreview
        ]


previewPanelSubscriptions : PreviewPanelModel -> Sub PreviewPanelMsg
previewPanelSubscriptions { dragState } =
    Draggable.subscriptions PreviewPanelDragMsg dragState
