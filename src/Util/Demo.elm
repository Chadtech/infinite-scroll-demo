module Util.Demo exposing
    ( buildings
    , getBuildings
    )

import Building exposing (Building)
import Process
import Random
import Task


wait : msg -> Cmd msg
wait msg =
    Process.sleep 2048
        |> Task.perform (\_ -> msg)


getBuildings : { from : Int, len : Int } -> (Result Never (List Building) -> msg) -> Cmd msg
getBuildings { from, len } toMsg =
    wait
        (List.drop from buildings
            |> List.take len
            |> Ok
            |> toMsg
        )


buildings : List Building
buildings =
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
    manyBuildings seed 10
