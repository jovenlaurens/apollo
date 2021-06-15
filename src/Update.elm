port module Update exposing (..)

import Html exposing (th)
import Html.Attributes exposing (list)
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))
import Model exposing (..)
import Star exposing (Point, Proton, Spacecraft, availableScale, originX, originY, spcheight, spcwidth, tracradius)
import Svg.Attributes exposing (mode)


port save : String -> Cmd msg


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 2 model) )



--in the update part, spacescraft is exposed as spc


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatespc msg


updatespc : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatespc msg ( model, cmd ) =
    case msg of
        Reinit level ->
            ( reinitModel level model, Cmd.none )

        Tick elapsed ->
            { model | move_timer = model.move_timer + elapsed }
                |> protonbounce
                |> spcmove
                |> checkoutearth
                |> earthmove
                |> protonmove
                |> checkPass
                |> checkfailed
                |> checkAddProton model.move_timer
                |> saveToStorage

        Start ->
            ( { initial | state = Playing }, Cmd.none )

        Key keydir ->
            ( model
                |> spcdirchange keydir
            , cmd
            )

        Pause ->
            saveToStorage { model | state = Paused }

        Resume ->
            ( { model | state = Playing }
            , Cmd.none
            )


checkAddProton : Float -> Model -> Model
checkAddProton time model =
    if model.level >= 0 then
        let
            old_proton =
                model.proton
        in
        if modBy 1000 (round time) == 0 then
            { model | proton = List.append old_proton initial.proton }

        else
            model

    else
        model


checkPass : Model -> Model
checkPass model =
    let
        nproton =
            List.filter (\a -> a.intensity > 0) model.proton
    in
    if model.heart <= 0 then
        reinitModel model.level model

    else if List.length nproton <= 0 then
        reinitModel (model.level + 1) model

    else
        model


reinitProton : Int -> List Proton -> List Proton
reinitProton inten list =
    case list of
        x :: xs ->
            let
                proton =
                    Proton (Point 400 300) 0.9 7.5 2.0 5

                nproton =
                    { proton | intensity = inten }

                --从proton的库里随机选一个proton，然后继承原本的intensity,回头写
            in
            nproton :: xs

        _ ->
            list


reinitModel : Int -> Model -> Model
reinitModel level model =
    let
        prototype =
            initial

        proEarth =
            initial.earth
    in
    case level of
        1 ->
            prototype

        2 ->
            { prototype
                | level = 2
                , spacecraft = addSpacecraft model.spacecraft
                , state = Paused
                , proton = prototype.proton
                , earth = { proEarth | pos = Point 700.0 500.0, show = Still, velocity = 0, radius = 0 } --earth的参数需要修改
            }

        3 ->
            { prototype
                | level = 3
                , state = Paused
                , proton = prototype.proton
                , earth = { proEarth | pos = Point 500.0 500.0, show = Move, velocity = 0.02, radius = 3.0 } --earth的参数需要修改
            }

        4 ->
            { prototype
                | level = 4
                , state = Paused
                , proton = prototype.proton
                , earth = { proEarth | pos = Point 500.0 500.0, show = Move, velocity = 0.02, radius = 3.0 } --earth的参数需要修改
            }

        _ ->
            prototype


addSpacecraft : List Spacecraft -> List Spacecraft
addSpacecraft spacecraft =
    spacecraft ++ List.singleton (Spacecraft (Point 900.0 500.0) 0.0 (Key_none 2) 0.01)


spcdirchange : Keydir -> Model -> Model
spcdirchange keydir model =
    if model.level > 1 then
        if keydir == Key_left 2 || keydir == Key_right 2 || keydir == Key_none 2 then
            { model | spacecraft = spcdirchange_inside keydir 2 model.spacecraft }

        else
            { model | spacecraft = spcdirchange_inside keydir 1 model.spacecraft }

    else
        { model | spacecraft = spcdirchange_inside keydir 1 model.spacecraft }


spcdirchange_inside : Keydir -> Int -> List Spacecraft -> List Spacecraft
spcdirchange_inside keydir index list =
    if index == 1 then
        case list of
            x :: xs ->
                { x | dir = keydir } :: xs

            _ ->
                list

    else
        case list of
            x :: xs ->
                let
                    old =
                        List.drop 1 list |> List.head |> Maybe.withDefault defaultSpacecraft
                in
                x :: ({ old | dir = keydir } |> List.singleton)

            _ ->
                list


spcmove : Model -> Model
spcmove model =
    { model | spacecraft = List.map renewSpc model.spacecraft }


renewSpc : Spacecraft -> Spacecraft
renewSpc spacecraft =
    let
        newangle =
            spcangle spacecraft.angle spacecraft.velocity spacecraft.dir

        newPoint =
            spcpostrans newangle
    in
    { spacecraft | pos = newPoint, angle = newangle }


spcangle : Float -> Float -> Keydir -> Float
spcangle angle velocity direction =
    case direction of
        Key_right a ->
            angle - velocity

        Key_left a ->
            angle + velocity

        Key_none a ->
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
    { model | proton = List.map move model.proton }


move : Proton -> Proton
move element =
    let
        direction =
            element.dir

        velocity =
            element.velocity

        point =
            element.pos

        movePoint : Point -> Point
        movePoint oldpoint =
            { oldpoint | x = oldpoint.x + velocity * cos direction, y = oldpoint.y + velocity * sin direction }
    in
    { element | pos = movePoint point }


checkfailed : Model -> Model
checkfailed model =
    List.foldr checkfailedInside model model.proton


checkfailedInside : Proton -> Model -> Model
checkfailedInside proton model =
    let
        dis =
            distance proton.pos (Point originX originY)

        old_heart =
            model.heart
    in
    if dis >= tracradius + 20 then
        { model | heart = old_heart - 1, proton = reinitProton proton.intensity model.proton, state = Paused }

    else
        model


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
    { model | proton = List.map (checkoutsunInside model) model.proton }


checkoutsunInside : Model -> Proton -> Proton
checkoutsunInside model proton =
    let
        posp =
            proton.pos

        poss =
            model.sun.pos

        rp =
            proton.radius

        rs =
            model.sun.radius

        a =
            proton.dir

        b =
            atan ((posp.y - poss.y) / (posp.x - poss.x))

        newdir =
            pi - a + 2 * b
    in
    if distance posp poss <= (rp + rs) then
        let
            old_intensity =
                proton.intensity
        in
        { proton | dir = newdir, intensity = old_intensity - 1 }

    else
        proton


anglebtwpoint : Point -> Point -> Float
anglebtwpoint pa pb =
    atan2 (pb.y - pa.y) (pa.x - pb.x)


checkoutspc : Model -> Model
checkoutspc model =
    List.foldr renewProtonDir model model.spacecraft


renewProtonDir : Spacecraft -> Model -> Model
renewProtonDir spacecraft model =
    let
        newproton =
            List.map (renewProntonDirInside spacecraft) model.proton
    in
    { model | proton = newproton }


renewProntonDirInside : Spacecraft -> Proton -> Proton
renewProntonDirInside spacecraft proton =
    let
        an =
            spacecraft.angle

        field =
            tan -an |> atan

        --spacecraft的范围,确保在-pi到pi之间
        special =
            atan ((proton.pos.y - originY) / (proton.pos.x - originX))

        ( a, b, c ) =
            getLine spacecraft.pos spacecraft.angle

        --得到spacecraft所在重心的那条切线，以ax+by+c=0的形式，记为l1
        distance_ =
            dotLineDistance proton.pos a b c

        --proton圆心到l1的距离
        stand =
            proton.radius + (0.5 * spcheight)

        --极限距离：proton半径+半个spacecraft厚度
    in
    if special <= field + availableScale + 0.05 && special >= field - availableScale - 0.05 && distance_ <= stand then
        let
            di =
                proton.dir

            newangle =
                pi - 2 * an - di
        in
        { proton | dir = newangle }

    else
        proton



--slope 斜率


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
                -1 / ((y - 500) / (x - 500))

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


earthmove : Model -> Model
earthmove model =
    let
        newangle =
            earthangle model.earth.angle model.earth.velocity

        newPoint =
            earthpostrans model.earth.pos newangle model.earth.radius

        earth_ =
            model.earth
    in
    { model | earth = { earth_ | angle = newangle, pos = newPoint } }


earthpostrans : Point -> Float -> Float -> Point
earthpostrans center angle radius =
    let
        posx =
            center.x + radius * cos angle

        posy =
            center.y - radius * sin angle
    in
    Point posx posy


earthangle : Float -> Float -> Float
earthangle angle velocity =
    angle + velocity


checkoutearth : Model -> Model
checkoutearth model =
    List.foldr checkoutearthInside model model.proton



--可能有问题


checkoutearthInside : Proton -> Model -> Model
checkoutearthInside proton model =
    let
        posp =
            proton.pos

        pose =
            model.earth.pos

        rp =
            proton.radius

        re =
            model.earth.radius
    in
    if distance posp pose <= (rp + re) then
        loseheart model

    else
        model


loseheart : Model -> Model
loseheart model =
    let
        heart_ =
            model.heart - 1
    in
    { model | heart = heart_ }
