module Update exposing (..)

import Messages exposing (Msg(..))
import Model exposing (Model)


updatepaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatepaddle msg ( model, cmd ) =
    case msg of
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
            , cmd
            )

        Key keydir ->
            ( model
            , cmd
            )
