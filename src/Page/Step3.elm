module Page.Step3 exposing
    ( Model
    , Msg
    , getSession
    , incomingPortsListener
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
import Ports.Incoming
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
    }


type Msg
    = Scrolled ScrollEvent
    | GotBuildings (Result Never (List Building))
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
            , scrollPosition = 0
            }
    in
    ( model
    , Demo.getBuildings
        { from = 0
        , len = pageSize
        }
        GotBuildings
    )



--------------------------------------------------------------------------------
-- HELPERS --
--------------------------------------------------------------------------------


pageSize : Int
pageSize =
    40



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
            let
                distanceToBottom : Float
                distanceToBottom =
                    event.scrollHeight - (event.scrollTop + event.elemHeight)

                modelWithScrollPos : Model
                modelWithScrollPos =
                    { model | scrollPosition = event.scrollTop }
            in
            if model.loadingMore || distanceToBottom > 700 then
                modelWithScrollPos
                    |> CmdUtil.withNone

            else
                Tuple.pair
                    { modelWithScrollPos | loadingMore = True }
                    (Demo.getBuildings
                        { from = List.length model.buildings
                        , len = pageSize
                        }
                        GotBuildings
                    )

        GotBuildings result ->
            case result of
                Ok moreBuildings ->
                    Tuple.pair
                        { model
                            | buildings = model.buildings ++ moreBuildings
                            , loadingMore = False
                            , renderFrom = model.renderFrom + (pageSize // 2)
                        }
                        (Browser.Dom.setViewportOf
                            scrollContainerId
                            10
                            0
                            |> Task.attempt SetScroll
                        )

                Err _ ->
                    model
                        |> CmdUtil.withNone

        SetScroll _ ->
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
                    , S.spaceBetween
                    ]
                ]
                [ Button.simple "Step 2"
                    |> Button.asLink (Route.step 2)
                    |> Button.toHtml
                , Button.simple "Step 4"
                    |> Button.asLink (Route.step 4)
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
                List.indexedMap buildingRow model.buildings ++ loading
    in
    Html.div
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


buildingRow : Int -> Building -> Html msg
buildingRow index building =
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
    in
    Html.div
        [ Attr.css
            [ bgColor
            , S.p 4
            , S.flex
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
            [ Html.text building.name ]
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



--------------------------------------------------------------------------------
-- PORTS --
--------------------------------------------------------------------------------


incomingPortsListener : Ports.Incoming.Listener Msg
incomingPortsListener =
    Ports.Incoming.none
