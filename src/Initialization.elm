module Initialization exposing (..)

import Messages exposing (..)
import Model exposing (..)



--Initialization


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    Model (generatebricks ( 10, 5 )) 0 (Paddle 50.0 None) (Ball 100.0 400.0 45.0 10)


generateonebrick : ( Int, Int ) -> Brick
generateonebrick position =
    Brick (brick_width * Tuple.first position + 2) (brick_height * Tuple.second position + 2)


generatebricks : ( Int, Int ) -> List Brick
generatebricks size =
    let
        rangex =
            List.range 0 (Tuple.first size - 1)

        rangey =
            List.range 0 (Tuple.second size - 1)

        line =
            \y -> List.map (\x -> Tuple.pair x y) rangex
    in
    List.map line rangey
        |> List.concat
        |> List.map generateonebrick
