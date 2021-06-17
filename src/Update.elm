port module Update exposing (update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))
import Model exposing (Model, State(..), defaultSpacecraft, initial)
import Random exposing (..)
import Star exposing (Earth, Point, Proton, Spacecraft, Sun, availableScale, originX, originY, sunRotateSpeed, tracradius)
import Text exposing (changeIndexToNewOne)


port save : String -> Cmd msg


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 2 model) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatespc msg


updatespc : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatespc msg ( model, cmd ) =
    let
        sbm =
            model.submodel
    in
    case msg of
        Resize width height ->
            ( { model | size = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        Reinit level ->
            ( reinitModel level model, Cmd.none )

        ChangeText a b ->
            ( { model | submodel = { sbm | text_num = changeIndexToNewOne a b } }, Cmd.none )

        GetViewport { viewport } ->
            ( { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none
            )

        Tick elapsed ->
            let
                osun =
                    model.sun
            in
            { model
                | submodel = { sbm | move_timer = sbm.move_timer + elapsed }
                , sun = { osun | angle = osun.angle + sunRotateSpeed }
            }
                |> protonbounce
                |> spcmove
                |> checkoutearth
                |> earthmove
                |> protonmove
                |> checkPass
                |> checkfailed
                |> checkAddProton model.submodel.move_timer
                |> saveToStorage

        Start ->
            ( { initial | submodel = { sbm | state = Playing }, size = model.size }, Cmd.none )

        EnterGame ->
            ( { initial | submodel = { sbm | state = Stopped }, size = model.size }, Cmd.none )

        Key keydir ->
            ( model
                |> spcdirchange keydir
            , cmd
            )

        Pause ->
            saveToStorage { model | submodel = { sbm | state = Paused } }

        Resume ->
            ( { model | submodel = { sbm | state = Playing } }
            , Cmd.none
            )


checkAddProton : Float -> Model -> Model
checkAddProton time model =
    if model.submodel.level == 4 then
        let
            old_proton =
                model.proton
        in
        if modBy 8000 (round time) >= 0 && modBy 8000 (round time) <= 100 && round (time / 8000) == List.length model.proton - 1 then
            { model | proton = List.append old_proton initial.proton }

        else
            model

    else
        model


checkPass : Model -> Model
checkPass model =
    let
        sbm =
            model.submodel

        nproton =
            List.filter (\a -> a.intensity > 0) model.proton

        ( add, trigue ) =
            if sbm.heart <= 0 then
                --这关gg了要重开
                ( 0, 2 )

            else if List.length nproton <= 0 then
                --赢了，下一关
                ( 1, 1 )

            else
                ( -99, -99 )

        nmodel =
            reinitModel (sbm.level + add) model

        nsbm =
            nmodel.submodel
    in
    { nmodel | submodel = { nsbm | text_num = changeIndexToNewOne model.submodel.text_num trigue } }


reinitProton : Random.Seed -> Int -> List Proton -> ( List Proton, Random.Seed )
reinitProton seed inten list =
    case list of
        x :: xs ->
            let
                ( proton, nseed ) =
                    generateNewProton seed

                nproton =
                    { proton | intensity = inten }

                --从proton的库里随机选一个proton，然后继承原本的intensity,回头写
            in
            ( nproton :: xs, nseed )

        _ ->
            ( list, seed )



--这里是proton的库


generateNewProton : Random.Seed -> ( Proton, Random.Seed )
generateNewProton seed =
    let
        ( index, nseed ) =
            Random.step (Random.int 1 4) seed

        protonBox =
            [ Proton (Point 300 300) 0.2 7.5 2.0 5 --need to be improved
            , Proton (Point 300 500) -1 10 2.0 8
            , Proton (Point 400 200) 1.5 8 1.5 5
            , Proton (Point 600 200) -1 10 2.0 5
            ]

        nproton =
            List.take index protonBox |> List.reverse |> List.head |> Maybe.withDefault (Proton (Point 300 500) -1 10 2.0 8)
    in
    ( nproton, nseed )


reinitModel : Int -> Model -> Model
reinitModel level model =
    if level < -10 then
        model

    else
        let
            prototype =
                initial

            prosize =
                model.size

            proEarth =
                initial.earth

            prosbm =
                initial.submodel
        in
        case level of
            1 ->
                { prototype | size = prosize, submodel = { prosbm | state = Paused } }

            2 ->
                { prototype
                    | submodel = { prosbm | level = 2, state = Paused }
                    , spacecraft = addSpacecraft model.spacecraft
                    , proton = prototype.proton
                    , earth = { proEarth | pos = Point 700.0 500.0, show = Still, velocity = 0, radius = 0 } --earth的参数需要修改
                    , size = prosize
                }

            3 ->
                { prototype
                    | submodel = { prosbm | level = 3, state = Paused }
                    , spacecraft = addSpacecraft model.spacecraft
                    , proton = prototype.proton
                    , earth = { proEarth | pos = Point 700.0 700.0, show = Move, velocity = 0.01, radius = 200.0 } --earth的参数需要修改
                    , size = prosize
                }

            4 ->
                { prototype
                    | submodel = { prosbm | level = 4, state = Paused }
                    , spacecraft = addSpacecraft model.spacecraft
                    , proton = prototype.proton
                    , earth = { proEarth | pos = Point 700.0 700.0, show = Move, velocity = 0.01, radius = 200.0 } --earth的参数需要修改
                    , size = prosize
                }

            _ ->
                prototype


addSpacecraft : List Spacecraft -> List Spacecraft
addSpacecraft spacecraft =
    spacecraft ++ List.singleton (Spacecraft (Point 300.0 500.0) pi (Key_none 2) 0.02)


spcdirchange : Keydir -> Model -> Model
spcdirchange keydir model =
    if model.submodel.level > 1 then
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

        osbm =
            model.submodel

        old_heart =
            osbm.heart

        ( nproton, nseed ) =
            reinitProton model.seed proton.intensity model.proton

        newSubmodel =
            { osbm | heart = old_heart - 1, state = Paused }
    in
    if dis >= tracradius + 20 then
        { model | proton = nproton, seed = nseed, submodel = newSubmodel }

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

        spcwing =
            spacecraft.pos

        leftwing =
            { spcwing | x = xShift spacecraft.pos.x -25, y = yShift spacecraft.pos.y -25 }

        rightwing =
            { spcwing | x = xShift spacecraft.pos.x 25, y = yShift spacecraft.pos.y 25 }

        ( a1, b1, c1 ) =
            getLine leftwing spacecraft.angle

        ( a2, b2, c2 ) =
            getLine rightwing spacecraft.angle

        --得到spacecraft所在重心的那条切线，以ax+by+c=0的形式，记为l1
        d1 =
            dotLineDistance proton.pos a1 b1 c1

        d2 =
            dotLineDistance proton.pos a2 b2 c2

        --proton圆心到l1的距离
        --极限距离：proton半径+半个spacecraft厚度
    in
    if special <= field + availableScale + 0.05 && special >= field - availableScale - 0.05 && (d1 <= proton.radius || d2 <= proton.radius) then
        let
            di =
                proton.dir

            newangle =
                pi - 2 * an - di

            _ =
                collideSound
        in
        { proton | dir = newangle }

    else
        proton


collideSound : Html Msg
collideSound =
    audio
        [ src "assets/Collide.wav"
        , autoplay True
        ]
        [ text "error" ]



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
                -(x - originX) / (y - originY)

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


xShift : Float -> Float -> Float
xShift x r =
    originX + (((tracradius + r) / tracradius) * (x - originX))


yShift : Float -> Float -> Float
yShift y r =
    originY - (((tracradius + r) / tracradius) * (originY - y))


earthmove : Model -> Model
earthmove model =
    if model.submodel.level > 2 then
        let
            newangle =
                earthangle model.earth.angle model.earth.velocity

            newPoint =
                earthpostrans newangle model.earth.radius

            earth_ =
                model.earth
        in
        { model | earth = { earth_ | angle = newangle, pos = newPoint } }

    else
        model


earthpostrans : Float -> Float -> Point
earthpostrans angle radius =
    let
        posx =
            originX + radius * cos angle

        posy =
            originY - radius * sin angle
    in
    Point posx posy


earthangle : Float -> Float -> Float
earthangle angle velocity =
    angle + velocity


checkoutearth : Model -> Model
checkoutearth model =
    List.foldr checkoutearthInside model model.proton



--a little issue


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
            30.0
    in
    if distance posp pose <= (rp + re) then
        loseheart model

    else
        model


loseheart : Model -> Model
loseheart model =
    let
        sbm =
            model.submodel

        heart_ =
            sbm.heart - 1
    in
    { model | submodel = { sbm | heart = heart_ } }
