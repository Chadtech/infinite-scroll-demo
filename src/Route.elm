module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , step
    )

import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Url exposing (Url)
import Url.Builder
import Url.Parser as P exposing ((</>), Parser)



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type Route
    = Landing
    | Step Int



---------------------------------------------------------------
-- API --
---------------------------------------------------------------


fromUrl : Url -> Maybe Route
fromUrl =
    P.parse parser


href : Route -> Html.Attribute msg
href route =
    Attr.href <| toString route


step : Int -> Route
step =
    Step



---------------------------------------------------------------
-- INTERNAL HELPERS --
---------------------------------------------------------------


toString : Route -> String
toString route =
    let
        path : List String
        path =
            case route of
                Landing ->
                    []

                Step int ->
                    [ String.fromInt int ]
    in
    Url.Builder.absolute path []


parser : Parser (Route -> a) a
parser =
    [ P.map Landing P.top
    , P.map Step (P.top </> P.int)
    ]
        |> P.oneOf
