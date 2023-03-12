module Building exposing
    ( Building
    , dummy
    )

import Random exposing (Seed)


type alias Building =
    { id : String
    , name : String
    }


dummy : Seed -> ( List Building, Seed )
dummy seed =
    let
        randInt : Random.Generator Int
        randInt =
            Random.int 0 9999999999

        fromIds : List String -> List Building
        fromIds ids =
            let
                make : String -> String -> Building
                make id name =
                    { id = id, name = name }
            in
            List.map2
                make
                ids
                [ "Gammage Auditorium"
                , "State Theater"
                , "The Nile Theater"
                , "AT&T Long Lines"
                , "Lincoln Center"
                , "The Cloisters"
                , "Stroud Mall"
                , "Rockaway Mall"
                , "Vollcorner"
                , "Park Place"
                , "Carpenters Training Center"
                , "Solomon Art Gallery"
                , "Impact Fun Zone"
                , "Ace Hotel"
                , "American Killer Bees"
                , "Crucible Jiu Jitsu"
                , "Yusan"
                , "The Chrystler Building"
                , "John Jay Park"
                , "The Met"
                , "Hasayampa"
                , "Fiesta Mall"
                , "Sun Ray Park"
                , "Alter Torbogen"
                , "Finanzamt München"
                , "Weinerwald"
                , "You Little"
                , "Schloss Nymphenburg"
                , "Steinheil 16"
                ]
    in
    Random.step
        (Random.list 20 (Random.map2 (\i j -> String.fromInt i ++ String.fromInt j) randInt randInt))
        seed
        |> Tuple.mapFirst fromIds
