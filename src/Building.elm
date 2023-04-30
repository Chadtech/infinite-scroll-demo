module Building exposing
    ( Building
    , color
    , randomGenerator
    )

import Hex
import Random exposing (Seed)


type alias Building =
    { id : Id
    , name : String
    , description : String
    }


type Id
    = Id Int Int Int


color : Building -> String
color building =
    let
        (Id idInt0 idInt1 idInt2) =
            building.id

        toColor : Int -> String
        toColor int =
            let
                str =
                    Hex.toString (int |> modBy 256)
            in
            if String.length str == 1 then
                "0" ++ str

            else
                str

        red =
            toColor idInt0

        green =
            toColor idInt1

        blue =
            toColor idInt2
    in
    "#" ++ red ++ green ++ blue


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
            , "Finanzamt"
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
            , "Kloster Andechs"
            , "Rustic Lodge"
            , "Stagger Lee"
            , "Prospect Park"
            , "LifeCycle WomanCare"
            ]

        from : List ( Id, ( Int, Int ) ) -> List Building
        from data =
            let
                make : ( Id, ( Int, Int ) ) -> String -> Building
                make ( id, ( descriptionStart, descriptionLength ) ) name =
                    { id = id
                    , name = name
                    , description =
                        exampleText
                            |> List.drop descriptionStart
                            |> List.take descriptionLength
                            |> String.join " "
                    }
            in
            List.map2 make data names
    in
    Random.list
        (List.length names)
        (Random.map2 Tuple.pair
            (Random.map3 Id anyInt anyInt anyInt)
            (Random.map2 Tuple.pair
                (Random.int 0 maxDescriptionLength)
                (Random.int 0 (maxDescriptionLength // 2))
            )
        )
        |> Random.map from
        |> Random.andThen shuffle


anyInt : Random.Generator Int
anyInt =
    Random.int Random.minInt Random.maxInt


maxDescriptionLength : Int
maxDescriptionLength =
    List.length exampleText


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


exampleText : List String
exampleText =
    "Maecenas eu sapien ut eros accumsan tempus vel ultricies quam. Proin vitae felis quis turpis convallis varius a eget lacus. Nunc laoreet, quam vitae convallis elementum, sapien nunc accumsan magna, et sagittis tortor tellus at ex. Nulla facilisi. Praesent auctor tellus lorem, in condimentum felis aliquet eget. In lacus quam, dictum sit amet odio ac, gravida cursus dolor. Maecenas tempus molestie diam. Nullam tristique neque maximus ante ornare pulvinar. Nunc a lacus eget leo semper lacinia in vitae nulla. Curabitur vitae dictum massa. Fusce arcu nunc, porta sit amet congue a, vehicula sed lectus. Pellentesque a enim nec arcu hendrerit vulputate. Mauris condimentum mauris et tempor sodales. Suspendisse egestas, dolor id imperdiet sollicitudin, nisl felis interdum ex, eu blandit velit urna quis metus. Suspendisse tincidunt elit sapien, vel egestas diam pretium eget. Nunc vehicula aliquam mi sed laoreet. Maecenas finibus leo at ligula varius, eu convallis elit accumsan. Integer in est tempor, laoreet turpis nec, auctor purus. Nam et varius felis. Proin viverra sem ut commodo blandit. In sit amet aliquam urna. Vestibulum ultrices, odio id semper dictum, dui ligula mattis est, at rutrum arcu nibh a sapien. Aliquam pretium neque vitae orci ullamcorper gravida nec et mauris. Aenean vehicula porttitor diam, quis facilisis justo varius id. In ornare porttitor lectus vel lobortis. Fusce egestas efficitur leo, et consequat nisi ornare ut. Morbi iaculis odio volutpat diam maximus, sit amet rhoncus diam egestas. Vivamus tristique ultrices arcu a imperdiet. Etiam venenatis, est quis placerat lacinia, eros eros mattis tellus, eget euismod urna nunc sed urna. Suspendisse dictum accumsan molestie. Interdum et malesuada fames ac ante ipsum primis in faucibus. Nulla egestas porttitor enim in fermentum. Pellentesque pharetra purus eu eleifend commodo. Mauris id mauris eget justo condimentum cursus. Pellentesque auctor odio id ultrices tincidunt. Maecenas sagittis dui in mi consectetur luctus. Nam pulvinar elit metus, in dignissim diam molestie a. Nullam sed nunc diam. Praesent dictum eget lectus vitae imperdiet. Etiam imperdiet feugiat velit sed commodo. Aenean quis convallis lacus. Ut auctor, ligula at bibendum ullamcorper, magna est pulvinar nunc, quis faucibus ex dolor et quam. Mauris eleifend ipsum nec arcu egestas dictum. Nunc tincidunt viverra sem, et condimentum metus mollis sit amet. Cras convallis nunc sem, id dictum tortor sodales at. Vivamus ac ligula tellus. Proin venenatis scelerisque sem pulvinar aliquam. Etiam malesuada efficitur odio sit amet lobortis. Ut ut justo ac dolor mattis pulvinar. Pellentesque efficitur nibh a magna molestie ornare. Quisque a ex malesuada ligula commodo mattis. Sed consectetur, ipsum eget cursus malesuada, nibh lectus lobortis urna, at maximus sem elit vel ante. Etiam elit nisl, eleifend nec sapien eu, feugiat posuere urna. Nulla eu tellus elementum, tincidunt dolor a, feugiat purus. Praesent viverra turpis eget nisi accumsan vehicula. Integer eleifend, leo vitae elementum congue, lorem elit bibendum ex, sit amet pellentesque turpis lacus eget leo. Nulla semper at lacus ac fermentum. Donec ligula lectus, bibendum sit amet ex id, sagittis volutpat ex. Mauris euismod tincidunt neque, quis elementum tellus ultricies vitae. Duis elementum bibendum diam, non auctor est tempor ac. Morbi ornare auctor enim, nec pulvinar ligula ornare sed. Sed consectetur quam in ligula varius imperdiet. Nunc sollicitudin, dui at lacinia egestas, mi tellus iaculis ex, sit amet pellentesque nulla neque eu arcu. Quisque pulvinar non tellus sit amet gravida. Mauris sodales neque purus, at condimentum massa aliquet id. Sed tempus, justo nec sollicitudin venenatis, neque purus accumsan dui, vitae dapibus justo ex sed diam. Maecenas urna turpis, ultrices eu enim quis, tincidunt dapibus lorem. In hac habitasse platea dictumst. Duis euismod dolor quam, eu pretium dolor egestas nec. Pellentesque sollicitudin volutpat velit, id luctus magna pellentesque id. Nullam tempus fermentum venenatis. Integer molestie libero elit, non fringilla sapien ultricies non. Nulla interdum odio lacus, fringilla auctor nisi tristique non. Praesent volutpat est commodo, lacinia lorem lobortis, vulputate sem."
        |> String.split " "
