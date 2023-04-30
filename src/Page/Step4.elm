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
import Json.Decode as JD exposing (Decoder)
import Layout exposing (Document)
import Route
import Session exposing (Session)
import Style as S
import Task
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
    , scrollPosition : Float
    , overscroll : Maybe ( Overscroll, Int )
    }


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


type Overscroll
    = Top
    | Bottom



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
            , scrollPosition = 0
            , overscroll = Nothing
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


incrOverscroll : Overscroll -> Model -> Model
incrOverscroll overscrollDir model =
    let
        nextOverscrollCount : Int
        nextOverscrollCount =
            case model.overscroll of
                Just ( _, c ) ->
                    c + 1

                Nothing ->
                    0
    in
    { model
        | overscroll = Just ( overscrollDir, nextOverscrollCount )
    }


pageSize : Int
pageSize =
    64


halfPageSize : Int
halfPageSize =
    pageSize // 2


bufferDistance : Float
bufferDistance =
    512


scrollAdjustment : Float
scrollAdjustment =
    toFloat halfPageSize * 64


incrRenderFrom : Model -> Model
incrRenderFrom model =
    { model
        | renderFrom = model.renderFrom + halfPageSize
    }
        |> incrOverscroll Bottom


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
            ( { model | renderFrom = max 0 (model.renderFrom - halfPageSize) }
                |> incrOverscroll Top
            , adjustScroll model -scrollAdjustment
            )

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
                , len = pageSize
                }
                GotBuildings
            )

        else
            ( model
                |> incrRenderFrom
            , adjustScroll model scrollAdjustment
            )


acceptBuildings : List Building -> Model -> Model
acceptBuildings buildings model =
    { model
        | buildings = model.buildings ++ buildings
        , loadingMore = False
    }


adjustScroll : Model -> Float -> Cmd Msg
adjustScroll model adjustment =
    Browser.Dom.setViewportOf
        scrollContainerId
        0
        (model.scrollPosition - adjustment)
        |> Task.attempt SetScroll



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
            handleScroll event { model | scrollPosition = event.scrollTop }

        GotBuildings result ->
            case result of
                Ok moreBuildings ->
                    ( model
                        |> incrRenderFrom
                        |> acceptBuildings moreBuildings
                    , adjustScroll model scrollAdjustment
                    )

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
                        |> CmdUtil.withNone

                Err error ->
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
            [ scrollContainer model
            ]
        ]


scrollContainerId : String
scrollContainerId =
    "scroll-container"


scrollContainer : Model -> Html Msg
scrollContainer model =
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
            if List.isEmpty model.buildings then
                loading

            else
                (model.buildings
                    |> List.indexedMap Tuple.pair
                    |> List.drop model.renderFrom
                    |> List.take pageSize
                    |> List.map buildingRow
                )
                    ++ loading
    in
    Html.node "infinite-scroller"
        [ Attr.css
            [ S.indent
            , S.w 10
            , S.hFull
            , S.bgBackground1
            , S.scroll
            ]
        , Ev.on "scroll" scrollDecoder
        , Attr.id scrollContainerId
        ]
        body


loading : List (Html msg)
loading =
    [ Html.div
        [ Attr.css
            [ S.flex
            , S.justifyCenter
            , S.p 6
            ]
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
            building.name ++ " - " ++ String.fromInt index
    in
    Html.div
        [ Attr.css
            [ bgColor
            , S.p 4
            , S.column
            , S.g 4
            ]
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
