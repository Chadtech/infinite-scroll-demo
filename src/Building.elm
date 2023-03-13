module Building exposing
    ( Building
    , randomGenerator
    )

import Random exposing (Seed)


type alias Building =
    { id : Id
    , name : String
    }


type Id
    = Id Int Int Int


randomGenerator : Random.Generator (List Building)
randomGenerator =
    let
        names : List String
        names =
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
            , "The Guggenheim"
            , "Hauptbahnhof"
            , "Apex Arena"
            , "Bank One Ballpark"
            , "Sea World"
            , "Rathaus"
            , "The Clubhouse"
            , "Rialto Theater"
            , "Cornish Pasty"
            , "LoFi Coffee"
            , "Cartel Coffee"
            , "Chuckbox"
            , "Art Annex"
            , "Discovery hall"
            , "Earthlight"
            , "Tempe Farmers Market"
            , "Studio 710 Apartments"
            , "Native New Yorker"
            ]

        fromIds : List Id -> List Building
        fromIds ids =
            let
                make : Id -> String -> Building
                make id name =
                    { id = id, name = name }
            in
            List.map2 make ids names
    in
    Random.list
        (List.length names)
        (Random.map3 Id anyInt anyInt anyInt)
        |> Random.map fromIds
        |> Random.andThen shuffle


anyInt : Random.Generator Int
anyInt =
    Random.int Random.minInt Random.maxInt


shuffle : List a -> Random.Generator (List a)
shuffle list =
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step anyInt seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed
