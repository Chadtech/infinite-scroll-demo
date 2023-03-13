module Style exposing (fontSize, globals, p)

import Chadtech.Colors as Ct
import Css exposing (Style)
import Css.Global exposing (global)
import Html.Styled exposing (Html)


globals : Html msg
globals =
    [ Css.Global.button
        [ hfnss
        , fontSmoothingNone
        , Css.outline Css.none
        , minWidth 6
        , Css.boxSizing Css.borderBox
        , Css.cursor Css.pointer
        , p 3
        ]
    , Css.Global.body
        [ Css.backgroundColor Ct.content1
        , Css.displayFlex
        , Css.flexDirection Css.column
        ]
    , Css.Global.everything
        [ Css.boxSizing Css.borderBox
        , Css.margin Css.zero
        , Css.padding Css.zero
        , Css.color Ct.content4
        , fontSmoothingNone
        , hfnss
        ]
    ]
        |> global


hfnss : Css.Style
hfnss =
    [ Css.fontFamilies [ "HFNSS" ]
    , fontSize 5
    ]
        |> Css.batch


fontSmoothingNone : Css.Style
fontSmoothingNone =
    Css.property "-webkit-font-smoothing" "none"


fontSize : Int -> Css.Style
fontSize =
    Css.fontSize << px


size : Int -> Float
size factor =
    toFloat (2 ^ factor)


px : Int -> Css.Px
px =
    Css.px << size


minWidth : Int -> Css.Style
minWidth =
    Css.minWidth << px


p : Int -> Css.Style
p =
    Css.padding << px
