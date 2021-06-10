module Point exposing (Point, move)


type alias Point =
    { x : Float
    , y : Float
    }



--move anything has field pos


move : { a | pos : Point } -> { a | pos : Point }
move element =
    let
        point =
            element.pos

        movePoint : Point -> Point
        movePoint oldpoint =
            { oldpoint | x = oldpoint.x + 1, y = oldpoint.y + 1 }
    in
    { element | pos = movePoint point }



--bounce
--bounce : { a | pos : Point } -> { a | pos : Point }
