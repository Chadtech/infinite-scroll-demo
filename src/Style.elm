module Style exposing (bgBackground1, bgBackground2, column, flex, fontSize, g, globals, h, hFull, importantIndent, importantOutdent, indent, justifyCenter, justifyEnd, m, mb, mt, none, outdent, p, relative, scroll, spaceBetween, w)

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
        , hFull
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
    if factor == 0 then
        0

    else
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


w : Int -> Css.Style
w =
    Css.width << px


g : Int -> Css.Style
g s =
    Css.property "gap" ((String.fromFloat <| size s) ++ "px")


h : Int -> Css.Style
h =
    Css.height << px


hFull : Style
hFull =
    Css.height <| Css.pct 100


m : Int -> Style
m =
    Css.margin << px


mt : Int -> Style
mt =
    Css.marginTop << px


mb : Int -> Style
mb =
    Css.marginBottom << px


flex : Style
flex =
    Css.displayFlex


column : Style
column =
    Css.batch
        [ flex, Css.flexDirection Css.column ]


justifyCenter : Style
justifyCenter =
    Css.justifyContent Css.center


spaceBetween : Style
spaceBetween =
    Css.justifyContent Css.spaceBetween


justifyEnd : Style
justifyEnd =
    Css.justifyContent Css.flexEnd


bgBackground1 : Style
bgBackground1 =
    Css.backgroundColor Ct.background1


bgBackground2 : Style
bgBackground2 =
    Css.backgroundColor Ct.background2


scroll : Style
scroll =
    Css.overflow Css.auto


relative : Style
relative =
    Css.position Css.relative


indent : Css.Style
indent =
    [ Css.borderLeft3 (px 1) Css.solid Ct.content0
    , Css.borderTop3 (px 1) Css.solid Ct.content0
    , Css.borderRight3 (px 1) Css.solid Ct.content2
    , Css.borderBottom3 (px 1) Css.solid Ct.content2
    ]
        |> Css.batch


outdent : Css.Style
outdent =
    [ Css.borderLeft3 (px 1) Css.solid Ct.content2
    , Css.borderTop3 (px 1) Css.solid Ct.content2
    , Css.borderRight3 (px 1) Css.solid Ct.content0
    , Css.borderBottom3 (px 1) Css.solid Ct.content0
    ]
        |> Css.batch


importantOutdent : Css.Style
importantOutdent =
    [ Css.borderLeft3 (px 1) Css.solid Ct.important2
    , Css.borderTop3 (px 1) Css.solid Ct.important2
    , Css.borderRight3 (px 1) Css.solid Ct.important0
    , Css.borderBottom3 (px 1) Css.solid Ct.important0
    ]
        |> Css.batch


importantIndent : Css.Style
importantIndent =
    [ Css.borderLeft3 (px 1) Css.solid Ct.important0
    , Css.borderTop3 (px 1) Css.solid Ct.important0
    , Css.borderRight3 (px 1) Css.solid Ct.important2
    , Css.borderBottom3 (px 1) Css.solid Ct.important2
    ]
        |> Css.batch


none : Style
none =
    Css.batch []
