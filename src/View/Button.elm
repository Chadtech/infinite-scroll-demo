module View.Button exposing
    ( asLink
    , onClick
    , primary
    , simple
    , toHtml
    )

import Chadtech.Colors as Ct
import Css
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        )
import Html.Styled.Attributes as Attr
import Route exposing (Route)
import Style
import Svg.Styled.Events as Event



--------------------------------------------------------------------------------
-- TYPES --
--------------------------------------------------------------------------------


type alias Button msg =
    { onClick : Click msg
    , label : String
    , variant : Variant
    }


type Variant
    = Variant__Primary
    | Variant__Secondary


type Click msg
    = Click__None
    | Click__Event msg
    | Click__Link Route



--------------------------------------------------------------------------------
-- HELPERS --
--------------------------------------------------------------------------------


fromLabelAndVariant : String -> Variant -> Button msg
fromLabelAndVariant label variant =
    { onClick = Click__None
    , label = label
    , variant = variant
    }



--------------------------------------------------------------------------------
-- API --
--------------------------------------------------------------------------------


simple : String -> Button msg
simple label =
    fromLabelAndVariant label Variant__Secondary


primary : String -> Button msg
primary label =
    fromLabelAndVariant label Variant__Primary


asLink : Route -> Button msg -> Button msg
asLink route button =
    { button | onClick = Click__Link route }


onClick : msg -> Button msg -> Button msg
onClick msg button =
    { button | onClick = Click__Event msg }


toHtml : Button msg -> Html msg
toHtml button =
    let
        conditionalAttrs : List (Attribute msg)
        conditionalAttrs =
            [ case button.onClick of
                Click__Event msg ->
                    Just <| Event.onClick msg

                Click__None ->
                    Nothing

                Click__Link route ->
                    Just <| Route.href route
            ]
                |> List.filterMap identity

        baseAttrs : List (Attribute msg)
        baseAttrs =
            let
                variantStyles : List Css.Style
                variantStyles =
                    case button.variant of
                        Variant__Primary ->
                            [ Css.backgroundColor Ct.important1
                            , Css.color Ct.important4
                            , Style.importantOutdent
                            , Css.hover
                                [ Css.color Ct.important5
                                ]
                            , Css.active
                                [ Css.color Ct.important5
                                , Style.importantIndent
                                ]
                            ]

                        Variant__Secondary ->
                            [ Css.backgroundColor Ct.content1
                            , Css.color Ct.content4
                            , Style.outdent
                            , Css.hover
                                [ Css.color Ct.content5
                                ]
                            , Css.active
                                [ Css.color Ct.content5
                                , Style.indent
                                ]
                            ]

                clickStyles : List Css.Style
                clickStyles =
                    case button.onClick of
                        Click__Link _ ->
                            [ Css.textDecoration Css.none
                            , Style.p 3
                            ]

                        _ ->
                            []
            in
            [ Attr.css
                (variantStyles ++ clickStyles)
            ]

        tagName : String
        tagName =
            case button.onClick of
                Click__Link _ ->
                    "a"

                _ ->
                    "button"
    in
    Html.node tagName
        (baseAttrs ++ conditionalAttrs)
        [ Html.text button.label ]
