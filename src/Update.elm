port module Update exposing (..)

import Html.Attributes exposing (list)
import Messages exposing (Keydir(..), Msg(..))
import Model exposing (..)
import Star exposing (Point, Proton, Spacecraft, availableScale, originX, originY, spcheight, spcwidth, tracradius)
import Svg.Attributes exposing (mode)
import Html exposing (th)
import Messages exposing (Earth_State(..))


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
                |> checkPass--这个是不是最好做到msg里，我有这个想法quq
                |> checkfailed
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


checkPass : Model -> Model
checkPass model =
    if model.heart <= 0 then
        reinitModel (model.level) model
    else if model.proton.intensity <= 0 then
        reinitModel (model.level + 1) model

    else
        model


reinitProton : Int -> Proton
reinitProton inten = 
    let
        nproton = (Proton (Point 400 300) 0.9 7.5 2.0 5)
        --从proton的库里随机选一个proton，然后继承原本的intensity,回头写
    in
        {nproton | intensity = inten }

    




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
            { prototype | level = 2
                        , spacecraft = addSpacecraft model.spacecraft
                        , state = Paused 
                        , earth = {proEarth | show = Still, velocity = 0.1}--earth的参数需要修改
            }

        --关于proton的再生成方法有待商榷
        3 ->
            model

        4 ->
            model

        --这里需要一个随机生成proton的函数和计分的东西
        _ ->
            model


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

checkfailed : Model -> Model
checkfailed model =
    let
        dis = distance model.proton.pos (Point originX originY)
        old_heart = model.heart
    in
        if dis >= tracradius + 5 then
            {model | heart = old_heart - 1, proton = reinitProton model.proton.intensity, state = Paused} 
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
    let
        posp =
            model.proton.pos

        poss =
            model.sun.pos

        rp =
            model.proton.radius

        rs =
            model.sun.radius

        a =
            model.proton.dir

        b =
            atan ((posp.y - poss.y) / (posp.x - poss.x))

        newdir =
            pi - a + 2 * b
    in
    if distance posp poss <= (rp + rs) then
        let
            proton_ =
                model.proton
            old_intensity = model.proton.intensity
        in
        { model | proton = { proton_ | dir = newdir, intensity = old_intensity - 1 } }

    else
        model


anglebtwpoint : Point -> Point -> Float
anglebtwpoint pa pb =
    atan2 (pb.y - pa.y) (pa.x - pb.x)


checkoutspc :
    Model
    -> Model --一个函数全部解决！
checkoutspc model =
    List.foldr renewProtonDir model model.spacecraft


renewProtonDir : Spacecraft -> Model -> Model
renewProtonDir spacecraft model =
    let
        an =
            spacecraft.angle

        field =
            tan -an |> atan

        --spacecraft的范围,确保在-pi到pi之间
        special =
            atan ((model.proton.pos.y - originY) / (model.proton.pos.x - originX))

        ( a, b, c ) =
            getLine spacecraft.pos spacecraft.angle

        --得到spacecraft所在重心的那条切线，以ax+by+c=0的形式，记为l1
        distance_ =
            dotLineDistance model.proton.pos a b c

        --proton圆心到l1的距离
        stand =
            model.proton.radius + (0.5 * spcheight)

        --极限距离：proton半径+半个spacecraft厚度
    in
    if special <= field + availableScale + 0.05 && special >= field - availableScale - 0.05 && distance_ <= stand then
        let
            di =
                model.proton.dir

            proton_ =
                model.proton

            newangle =
                pi - 2 * an - di


        in
        { model | proton = { proton_ | dir = newangle } }

    else
        model



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
            earthpostrans newangle model.earth.radius
        earth_ = model.earth    
    in
        { model | earth = { earth_ | angle =newangle, pos = newPoint } }

earthpostrans : Float -> Float -> Point
earthpostrans angle  radius= 
        let
            posx = 
                    ( 500.0 + radius * cos (angle) )
            posy = 
                    ( 500.0 - radius * sin (angle) )
        in
            Point posx posy
earthangle : Float -> Float -> Float
earthangle angle velocity = 
            angle + velocity

checkoutearth : Model -> Model  
checkoutearth model =
    let
        posp = 
            model.proton.pos
        pose = 
            model.earth.pos
        rp = 
            model.proton.radius
        re = 
            model.earth.radius
    in
         if (distance posp pose) <= (rp + re)  then
                loseheart model
         else
            model     
loseheart : Model -> Model
loseheart model =
    let
        heart_ = model.heart - 1
        
    in
        {model | heart = heart_ }   