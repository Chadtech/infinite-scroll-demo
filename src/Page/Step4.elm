module Page.Step4 exposing
    ( Model
    , Msg
    , getSession
    , init
    , update
    , view
    )

import Browser.Dom
import Building exposing (Building)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Html.Styled.Lazy
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Layout exposing (Document)
import Route
import Session exposing (Session)
import Style as S
import Util.Cmd as CmdUtil
import Util.Demo as Demo
import View.Button as Button



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type alias Model =
    { session : Session
    , buildings : List Building
    , renderFrom : Int
    , loadingMore : Bool
    , recalculatePages : Int
    , shift : Maybe ( Direction, Int )
    }


type Direction
    = Up
    | Down


type Msg
    = Scrolled ScrollEvent
    | GotBuildings (Result Never (List Building))
    | GotFirstBuildings (Result Never (List Building))
    | SetScroll (Result Browser.Dom.Error ())


type alias ScrollEvent =
    { scrollHeight : Float
    , scrollTop : Float
    , elemHeight : Float
    }



--------------------------------------------------------------------------------
-- INIT --
--------------------------------------------------------------------------------


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model : Model
        model =
            { session = session
            , buildings = []
            , renderFrom = 0
            , loadingMore = True
            , recalculatePages = 0
            , shift = Nothing
            }
    in
    ( model
    , Demo.getBuildings
        { from = 0
        , len = pageSize
        }
        GotFirstBuildings
    )



--------------------------------------------------------------------------------
-- HELPERS --
--------------------------------------------------------------------------------


recalculatePages : Model -> Model
recalculatePages model =
    { model
        | recalculatePages = model.recalculatePages + 1
    }


pageSize : Int
pageSize =
    32


halfPageSize : Int
halfPageSize =
    pageSize // 2


bufferDistance : Float
bufferDistance =
    512


incrRenderFrom : Model -> Model
incrRenderFrom model =
    { model
        | renderFrom = model.renderFrom + halfPageSize
        , shift =
            case model.shift of
                Just ( _, c ) ->
                    Just ( Down, c + 1 )

                Nothing ->
                    Just ( Down, 0 )
    }


handleScroll : ScrollEvent -> Model -> ( Model, Cmd Msg )
handleScroll event model =
    let
        distanceToBottom : Float
        distanceToBottom =
            event.scrollHeight - (event.scrollTop + event.elemHeight)
    in
    if model.loadingMore || distanceToBottom > bufferDistance then
        let
            distanceToTop : Float
            distanceToTop =
                event.scrollTop
        in
        if distanceToTop < bufferDistance && model.renderFrom > 0 then
            { model
                | renderFrom = max 0 (model.renderFrom - halfPageSize)
                , shift =
                    case model.shift of
                        Just ( _, c ) ->
                            Just ( Up, c + 1 )

                        Nothing ->
                            Just ( Up, 0 )
            }
                |> CmdUtil.withNone

        else
            model
                |> CmdUtil.withNone

    else
        let
            amountWeNeed : Int
            amountWeNeed =
                model.renderFrom + (2 * pageSize)

            amountWeHave : Int
            amountWeHave =
                List.length model.buildings
        in
        if amountWeNeed > amountWeHave then
            ( { model | loadingMore = True }
            , Demo.getBuildings
                { from = List.length model.buildings
                , len = pageSize * 2
                }
                GotBuildings
            )

        else
            model
                |> incrRenderFrom
                |> CmdUtil.withNone


acceptBuildings : List Building -> Model -> Model
acceptBuildings buildings model =
    { model
        | buildings = model.buildings ++ buildings
        , loadingMore = False
    }



--------------------------------------------------------------------------------
-- API --
--------------------------------------------------------------------------------


getSession : Model -> Session
getSession model =
    model.session



--------------------------------------------------------------------------------
-- UPDATE --
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scrolled event ->
            handleScroll event model

        GotBuildings result ->
            case result of
                Ok moreBuildings ->
                    model
                        |> incrRenderFrom
                        |> acceptBuildings moreBuildings
                        |> CmdUtil.withNone

                Err _ ->
                    model
                        |> CmdUtil.withNone

        SetScroll _ ->
            model
                |> CmdUtil.withNone

        GotFirstBuildings result ->
            case result of
                Ok buildings ->
                    acceptBuildings buildings model
                        |> recalculatePages
                        |> CmdUtil.withNone

                Err _ ->
                    model
                        |> CmdUtil.withNone



--------------------------------------------------------------------------------
-- VIEW --
--------------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    Layout.document
        "Infinite Scroll"
        [ title
        , Html.div
            [ Attr.css
                [ S.flex
                , S.justifyCenter
                , S.mb 4
                ]
            ]
            [ Html.div
                [ Attr.css
                    [ S.flex
                    , S.w 10
                    ]
                ]
                [ Button.simple "Step 3"
                    |> Button.asLink (Route.step 3)
                    |> Button.toHtml
                ]
            ]
        , Html.div
            [ Attr.css
                [ S.flex
                , S.justifyCenter
                , S.m 4
                , S.mt 0
                , Css.flex <| Css.int 1
                , Css.minHeight Css.zero
                ]
            ]
            [ Html.Styled.Lazy.lazy4
                scrollContainer
                model.buildings
                model.renderFrom
                model.shift
                model.recalculatePages
            ]
        ]


scrollContainerId : String
scrollContainerId =
    "scroll-container"


scrollContainer : List Building -> Int -> Maybe ( Direction, Int ) -> Int -> Html Msg
scrollContainer buildings renderFrom shift recalculateCount =
    let
        scrollDecoder : Decoder Msg
        scrollDecoder =
            let
                eventDecoder : Decoder ScrollEvent
                eventDecoder =
                    JD.map3 ScrollEvent
                        (JD.field "scrollHeight" JD.float)
                        (JD.field "scrollTop" JD.float)
                        (JD.field "offsetHeight" JD.float)
                        |> JD.field "target"
            in
            eventDecoder
                |> JD.map Scrolled

        body : List (Html msg)
        body =
            if List.isEmpty buildings then
                loading "loading"

            else
                let
                    _ =
                        Debug.log "RENDERING" (List.length buildings)

                    sliceOfBuildings : List ( Int, Building )
                    sliceOfBuildings =
                        buildings
                            |> List.indexedMap Tuple.pair
                            |> List.drop (Debug.log "RENDER FROM" renderFrom)
                            |> List.take pageSize
                in
                (sliceOfBuildings
                    |> List.map buildingRow
                )
                    ++ loading "loading-bottom"

        shiftJson : JE.Value
        shiftJson =
            case shift of
                Just ( dir, c ) ->
                    let
                        dirStr : String
                        dirStr =
                            case dir of
                                Up ->
                                    "up"

                                Down ->
                                    "down"
                    in
                    JE.object
                        [ Tuple.pair "direction" <| JE.string dirStr
                        , Tuple.pair "count" <| JE.int c
                        ]

                Nothing ->
                    JE.null
    in
    Html.node "infinite-scroller"
        [ Attr.css
            [ S.indent
            , S.w 10
            , S.hFull
            , S.bgBackground1
            , S.scroll
            ]
        , Attr.attribute "recalculate" <| String.fromInt recalculateCount
        , Attr.attribute "shift" <| JE.encode 0 shiftJson
        , Attr.attribute "pageShiftSize" <| String.fromInt halfPageSize
        , Ev.on "scroll" scrollDecoder
        , Attr.id scrollContainerId
        ]
        body


loading : String -> List (Html msg)
loading id =
    [ Html.div
        [ Attr.css
            [ S.flex
            , S.justifyCenter
            , S.p 6
            ]
        , Attr.id id
        ]
        [ Html.text "Loading.."
        ]
    ]


buildingRow : ( Int, Building ) -> Html msg
buildingRow ( index, building ) =
    let
        bgColor : Css.Style
        bgColor =
            if (index |> modBy 2) == 0 then
                S.bgBackground2

            else
                S.none

        color : Css.Color
        color =
            Css.hex <| Building.color building

        label : String
        label =
            (building.name ++ " - " ++ String.fromInt index)
                |> Debug.log "Label in view"
    in
    Html.node "row"
        [ Attr.css
            [ bgColor
            , S.p 4
            , S.column
            , S.g 4
            ]
        , Attr.attribute "data-label" label
        ]
        [ Html.div
            [ Attr.css
                [ S.flex
                , S.g 4
                ]
            ]
            [ Html.div
                [ Attr.css
                    [ S.indent
                    , Css.backgroundColor color
                    , S.w 5
                    , S.h 5
                    ]
                ]
                []
            , Html.div
                [ Attr.css
                    [ S.column
                    , S.justifyCenter
                    ]
                ]
                [ Html.text label ]
            ]
        , Html.div
            []
            [ Html.text building.description ]
        ]


title : Html Msg
title =
    Html.div
        [ Attr.css
            [ S.p 4
            , S.fontSize 6
            ]
        ]
        [ Html.text "Infinite Scroll Demo" ]
