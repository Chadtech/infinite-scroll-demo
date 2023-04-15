module Page.Home exposing
    ( Model
    , Msg
    , getSession
    , incomingPortsListener
    , init
    , update
    , view
    )

import Building exposing (Building)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Json.Decode as JD exposing (Decoder)
import Layout exposing (Document)
import Ports.Incoming
import Random
import Session exposing (Session)
import Style as S
import Util.Cmd as CmdUtil



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type alias Model =
    { session : Session
    , buildings : List Building
    , viewUpTo : Int
    }


type Msg
    = ScrolledToBottom


type alias ScrollEvent =
    { scrollHeight : Float
    , scrollTop : Float
    , elemHeight : Float
    }



--------------------------------------------------------------------------------
-- INIT --
--------------------------------------------------------------------------------


init : Session -> Model
init session =
    let
        seed : Random.Seed
        seed =
            Random.initialSeed 2324

        manyBuildings : Random.Seed -> Int -> List Building
        manyBuildings seed0 howManyLists =
            if howManyLists > 0 then
                let
                    ( nextBuildings, seed1 ) =
                        Random.step Building.randomGenerator seed0
                in
                nextBuildings ++ manyBuildings seed1 (howManyLists - 1)

            else
                []
    in
    { session = session
    , buildings = manyBuildings seed 10
    , viewUpTo = pageSize
    }



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
        ScrolledToBottom ->
            { model | viewUpTo = model.viewUpTo + pageSize }
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
                , S.m 4
                , S.mt 0
                , Css.flex <| Css.int 1
                , Css.minHeight Css.zero
                ]
            ]
            [ scrollContainer model ]
        ]


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

                isCloseToBottom : ScrollEvent -> Decoder Msg
                isCloseToBottom event =
                    let
                        distanceToBottom : Float
                        distanceToBottom =
                            event.scrollHeight - (event.scrollTop + event.elemHeight)
                    in
                    if distanceToBottom < 700 then
                        JD.succeed ScrolledToBottom

                    else
                        JD.fail "Above scroll-to-bottom threshold"
            in
            eventDecoder
                |> JD.andThen isCloseToBottom
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
        ]
        (List.indexedMap buildingRow <|
            List.take model.viewUpTo model.buildings
        )


buildingRow : Int -> Building -> Html msg
buildingRow index building =
    let
        bgColor : Css.Style
        bgColor =
            if (index |> modBy 2) == 0 then
                S.bgBackground2

            else
                S.none
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
                , Css.backgroundColor <| Css.hex <| Building.color building
                , S.w 5
                , S.h 5
                ]
            ]
            []
        , Html.div
            [ Attr.css
                [ S.flexCol
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
