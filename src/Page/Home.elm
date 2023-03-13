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
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as Decode
import Layout exposing (Document)
import Ports.Incoming
import Ports.Outgoing
import Random
import Session exposing (Session)
import Style as S
import Util.Cmd as CmdUtil
import Util.Html as HtmlUtil



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type alias Model =
    { session : Session
    , buildings : List Building
    }


type Msg
    = Msg



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
        Msg ->
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
        , scrollContainer
        ]


scrollContainer : Html msg
scrollContainer =
    Html.div
        []
        []


title : Html Msg
title =
    Html.div
        [ Attrs.css
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
