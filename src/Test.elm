module Test exposing (..)

import Playground exposing (..)


main =
    picture
        [ square red 20
            |> move -200 250
        , square yellow 100
            |> move 60 60
        , square green 100
            |> move 60 -60
        , square blue 100
            |> move -60 -60
        ]
