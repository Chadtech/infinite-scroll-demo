module Util.Demo exposing (wait)

import Building exposing (Building)
import Process
import Task


wait : msg -> Cmd msg
wait msg =
    Process.sleep 2000
        |> Task.perform (\_ -> msg)


getBuildings : { pageFrom : Int } ->  (Result Never (List Building) -> msg) -> Cmd msg
getBuildings { pageFrom} toMsg =
     