module Session exposing
    ( Session
    , init
    , pushUrl
    )

import Browser.Navigation as Nav
import Url exposing (Url)



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type alias Session =
    { navKey : Nav.Key }



---------------------------------------------------------------
-- INIT --
---------------------------------------------------------------


init : Nav.Key -> Session
init navKey =
    { navKey = navKey }



---------------------------------------------------------------
-- API --
---------------------------------------------------------------


pushUrl : Session -> Url -> Cmd msg
pushUrl session url =
    Nav.pushUrl session.navKey (Url.toString url)
