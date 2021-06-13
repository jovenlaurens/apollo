module Update exposing (..)

import Messages exposing (Keydir(..), Msg(..))
import Model exposing (Model)
import Star exposing (Point, Spacecraft, originX, originY, spcheight, spcwidth, tracradius)
import Svg.Attributes exposing (mode)



--in the update part, spacescraft is exposed as spc


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatespc msg


updatespc : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatespc msg ( model, cmd ) =
    case msg of
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
                |> protonbounce
                |> spcmove
                |> protonmove
            , cmd
            )

        Key keydir ->
            ( model
                |> spcdirchange keydir
            , cmd
            )

        _ ->
            ( model, cmd )


spcdirchange : Keydir -> Model -> Model
spcdirchange keydir model =
    { model
        | spacecraft =
            { pos = model.spacecraft.pos
            , angle = model.spacecraft.angle
            , dir = keydir
            , velocity = model.spacecraft.velocity
            }
    }


spcmove : Model -> Model
spcmove model =
    let
        newangle =
            spcangle model.spacecraft.angle model.spacecraft.velocity model.spacecraft.dir

        newPoint =
            spcpostrans newangle
    in
    { model | spacecraft = { pos = newPoint, angle = newangle, dir = model.spacecraft.dir, velocity = model.spacecraft.velocity } }


spcangle : Float -> Float -> Keydir -> Float
spcangle angle velocity direction =
    case direction of
        Key_right ->
            angle - velocity

        Key_left ->
            angle + velocity

        Key_none ->
            angle


spcpostrans : Float -> Point
spcpostrans angle =
    let
        posx =
            originX + tracradius * cos angle

        posy =
            originY - tracradius * sin angle
    in
    Point posx posy


protonmove : Model -> Model
protonmove model =
    { model | proton = move model.proton model.proton.dir model.proton.velocity }


move : { a | pos : Point } -> Float -> Float -> { a | pos : Point }
move element direction velocity =
    let
        point =
            element.pos

        movePoint : Point -> Point
        movePoint oldpoint =
            { oldpoint | x = oldpoint.x + velocity * cos direction, y = oldpoint.y + velocity * sin direction }
    in
    { element | pos = movePoint point }


protonbounce : Model -> Model
protonbounce model =
    model
        |> checkoutsun
        |> checkoutspc


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


checkoutsun : Model -> Model
checkoutsun model =
    let
        posp =
            model.proton.pos

        poss =
            model.sun.pos

        rp =
            model.proton.radius

        rs =
            model.sun.radius

        ap2s =
            anglebtwpoint posp poss

        ap =
            model.proton.dir

        newdir =
            (2 * ap2s) - pi - ap
    in
    if distance posp poss <= (rp + rs) then
        let
            proton_ =
                model.proton
        in
        { model | proton = { proton_ | dir = newdir } }

    else
        model


anglebtwpoint : Point -> Point -> Float
anglebtwpoint pa pb =
    atan2 (pb.y - pa.y) (pa.x - pb.x)


checkoutspc :
    Model
    -> Model --一个函数全部解决！
checkoutspc model =
    let
        ( a, b, c ) =
            getLine model.spacecraft.pos model.spacecraft.angle

        --得到spacecraft所在重心的那条切线，以ax+by+c=0的形式，记为l1
        distance_ =
            dotLineDistance model.proton.pos a b c

        --proton圆心到l1的距离
        stand =
            model.proton.radius + (0.5 * spcheight)

        --极限距离：proton半径+半个spacecraft厚度
    in
    if distance_ <= stand then
        let
            di =
                model.proton.dir
            an = model.spacecraft.angle
            proton_ =
                model.proton

            newangle =
                pi - 2 * an - di
                --还是不对我哭了
                ---对了！！！
  
        in
        { model | proton = { proton_ | dir = newangle } }

    else
        model



--slope 斜率


pi : Float
pi =
    acos 0 * 2




getLine : Point -> Float -> ( Float, Float, Float )
getLine pos angle =
    if angle == 0 || angle == pi then
        (1, 0, -pos.x)
    else if angle == (pi * 1 / 2) || angle == (pi * 3 / 2) then
        (0, 1, -pos.y)
    else
    let
        x =
            pos.x

        y =
            pos.y
        a = -1 / ((y - 500) / (x - 500))


        b =
            -1


        c =
            y - a * x
    in
    ( a, b, c )


dotLineDistance : Point -> Float -> Float -> Float -> Float
dotLineDistance point a b c =
    let
        x =
            point.x

        y =
            point.y
    in
    abs (a * x + b * y + c) / sqrt (a ^ 2 + b ^ 2)
