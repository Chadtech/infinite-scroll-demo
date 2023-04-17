module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Layout exposing (Document)
import Page.Step1 as Step1
import Page.Step2 as Step2
import Page.Step3 as Step3
import Ports.Incoming
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Util.Cmd as CmdUtil



--------------------------------------------------------------------------------
-- MAIN --
--------------------------------------------------------------------------------


main : Program Decode.Value Model Msg
main =
    { init = init
    , view = Layout.toBrowserDocument << view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = UrlRequested
    , onUrlChange = RouteChanged << Route.fromUrl
    }
        |> Browser.application



--------------------------------------------------------------------------------
-- TYPES --
--------------------------------------------------------------------------------


type Model
    = PageNotFound Session
    | Step1 Step1.Model
    | Step2 Step2.Model
    | Step3 Step3.Model


type Msg
    = MsgDecodeFailed Ports.Incoming.Error
    | UrlRequested UrlRequest
    | RouteChanged (Maybe Route)
    | Step1Msg Step1.Msg
    | Step2Msg Step2.Msg
    | Step3Msg Step3.Msg



--------------------------------------------------------------------------------
-- INIT --
--------------------------------------------------------------------------------


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        session : Session
        session =
            Session.init navKey
    in
    PageNotFound session
        |> handleRouteChange (Route.fromUrl url)



--------------------------------------------------------------------------------
-- INTERNAL HELPERS --
--------------------------------------------------------------------------------


getSession : Model -> Session
getSession model =
    case model of
        PageNotFound session ->
            session

        Step1 subModel ->
            Step1.getSession subModel

        Step2 subModel ->
            Step2.getSession subModel

        Step3 subModel ->
            Step3.getSession subModel



--------------------------------------------------------------------------------
-- UPDATE --
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed _ ->
            model
                |> CmdUtil.withNone

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    Tuple.pair
                        model
                        (Session.pushUrl (getSession model) url)

                Browser.External string ->
                    model
                        |> CmdUtil.withNone

        RouteChanged maybeRoute ->
            handleRouteChange maybeRoute model

        Step1Msg subMsg ->
            case model of
                Step1 subModel ->
                    Step1.update subMsg subModel
                        |> Step1
                        |> CmdUtil.withNone

                _ ->
                    ( model, Cmd.none )

        Step2Msg subMsg ->
            case model of
                Step2 subModel ->
                    Step2.update subMsg subModel
                        |> CmdUtil.mapBoth Step2 Step2Msg

                _ ->
                    ( model, Cmd.none )

        Step3Msg subMsg ->
            case model of
                Step3 subModel ->
                    Step3.update subMsg subModel
                        |> CmdUtil.mapBoth Step3 Step3Msg

                _ ->
                    ( model, Cmd.none )


handleRouteChange : Maybe Route -> Model -> ( Model, Cmd Msg )
handleRouteChange maybeRoute model =
    let
        session =
            getSession model
    in
    case maybeRoute of
        Nothing ->
            PageNotFound session
                |> CmdUtil.withNone

        Just route ->
            case route of
                Route.Landing ->
                    Step1.init session
                        |> Step1
                        |> CmdUtil.withNone

                Route.Step step ->
                    case step of
                        1 ->
                            Step1.init session
                                |> Step1
                                |> CmdUtil.withNone

                        2 ->
                            Step2.init session
                                |> CmdUtil.mapBoth Step2 Step2Msg

                        3 ->
                            Step3.init session
                                |> CmdUtil.mapBoth Step3 Step3Msg

                        _ ->
                            PageNotFound session
                                |> CmdUtil.withNone



--------------------------------------------------------------------------------
-- VIEW --
--------------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    case model of
        PageNotFound _ ->
            Layout.document
                "Page not found"
                [ Html.text "Page not found!" ]

        Step1 subModel ->
            Step1.view subModel
                |> Layout.map Step1Msg

        Step2 subModel ->
            Step2.view subModel
                |> Layout.map Step2Msg

        Step3 subModel ->
            Step3.view subModel
                |> Layout.map Step3Msg



--------------------------------------------------------------------------------
-- SUBSCRIPTIONS --
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.Incoming.subscription
        MsgDecodeFailed
        (incomingPortsListeners model)


incomingPortsListeners : Model -> Ports.Incoming.Listener Msg
incomingPortsListeners model =
    case model of
        PageNotFound _ ->
            Ports.Incoming.none

        Step1 _ ->
            Ports.Incoming.map Step1Msg Step1.incomingPortsListener

        Step2 _ ->
            Ports.Incoming.map Step2Msg Step2.incomingPortsListener

        Step3 _ ->
            Ports.Incoming.map Step3Msg Step3.incomingPortsListener
