module Util.Cmd exposing
    ( mapBoth
    , withNone
    )


withNone : model -> ( model, Cmd msg )
withNone model =
    ( model, Cmd.none )


mapBoth : (subModel -> model) -> (subMsg -> msg) -> ( subModel, Cmd subMsg ) -> ( model, Cmd msg )
mapBoth modelF msgF ( subModel, cmd ) =
    ( modelF subModel
    , Cmd.map msgF cmd
    )
