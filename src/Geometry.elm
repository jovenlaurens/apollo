module Geometry exposing (Point, availableScale, distance, dotLineDistance, getLine, originX, originY, spcheight, spcwidth, sunRadius, sunRotateSpeed, tracradius, xShift, yShift)


type alias Point =
    { x : Float
    , y : Float
    }


sunRotateSpeed : Float
sunRotateSpeed =
    0.4


tracearth : Float
tracearth =
    150.0


tracradius : Float
tracradius =
    300.0


spcwidth : Float
spcwidth =
    60.0


spcheight : Float
spcheight =
    20.0


originX =
    500.0


originY =
    500.0


sunRadius =
    80.0


availableScale =
    atan ((0.5 * spcwidth) / tracradius)


dotLineDistance : Point -> Float -> Float -> Float -> Float
dotLineDistance point a b c =
    let
        x =
            point.x

        y =
            point.y
    in
    abs (a * x + b * y + c) / sqrt (a ^ 2 + b ^ 2)


getLine : Point -> Float -> ( Float, Float, Float )
getLine pos angle =
    if angle == 0 || angle == pi then
        ( 1, 0, -pos.x )

    else if angle == (pi * 1 / 2) || angle == (pi * 3 / 2) then
        ( 0, 1, -pos.y )

    else
        let
            x =
                pos.x

            y =
                pos.y

            a =
                -(x - originX) / (y - originY)

            b =
                -1

            c =
                y - a * x
        in
        ( a, b, c )


distance : Point -> Point -> Float
distance pa pb =
    let
        ax =
            pa.x

        ay =
            pa.y

        bx =
            pb.x

        by =
            pb.y
    in
    sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2)


xShift : Float -> Float -> Float
xShift x r =
    originX + (((tracradius + r) / tracradius) * (x - originX))


yShift : Float -> Float -> Float
yShift y r =
    originY - (((tracradius + r) / tracradius) * (originY - y))
