module Main exposing (..)

import Playground exposing (..)


main =
    game view update { x = 200, y = 200 }


initLineLocation : Number -> List ( Number, Number )
initLineLocation row =
    let
        addPoNumber : Number -> List ( Number, Number ) -> List ( Number, Number )
        addPoNumber elem lst =
            ( elem, row ) :: lst
    in
    List.foldr addPoNumber [] [ -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5 ]


initLocation : Number -> List ( Number, Number )
initLocation line =
    let
        addLine : Number -> List (List ( Number, Number )) -> List (List ( Number, Number ))
        addLine elm2 lst2 =
            initLineLocation elm2 :: lst2
    in
    List.foldr addLine [] [ 1, 2, 3, 4, 5 ]
        |> List.concat


initBricks : List ( Number, Number )
initBricks =
    let
        tran : ( Number, Number ) -> ( Number, Number )
        tran ( sx, sy ) =
            ( 30 * sx, 10 * sy + 245 )
    in
    List.map tran (initLocation 3)


brickSet : Color -> Shape
brickSet brickColor =
    let
        moveBrick : ( Number, Number ) -> List Shape -> List Shape
        moveBrick ( xd, yd ) lstBrick =
            rectangle yellow 25 8
                |> move xd yd
                |> List.repeat 1
                |> List.append lstBrick
    in
    List.foldr moveBrick [] initBricks
        |> group


view computer shapes =
    [ rectangle blue 50 20
        |> move 0 -300
        |> move shapes.x shapes.y
    , brickSet yellow
    ]


update computer shapes =
    { x = shapes.x + 3 * toX computer.keyboard
    , y = shapes.y + 3 * toY computer.keyboard
    }
