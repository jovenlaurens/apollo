port module Update exposing (update)

import Geometry exposing (Point, availableScale, distance, dotLineDistance, getLine, originX, originY, sunRotateSpeed, tracradius, xShift, yShift)
import Html exposing (..)
import Html.Attributes exposing (..)
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))
import Model exposing (Model, State(..), initial)
import Random exposing (..)
import Star exposing (Proton, Spacecraft, defaultSpacecraft)
import Text exposing (changeIndexToNewOne)


port save : String -> Cmd msg


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 3 model) )


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

        PlayInterval ->
            ( { initial | submodel = { sbm | state = Interval }, size = model.size }, Cmd.none )

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
            saveToStorage { model | submodel = { sbm | state = Paused, text_num = changeIndexToNewOne sbm.text_num 0 } }

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

            otime =
                modBy 8000 (round time)
        in
        if otime >= 0 && otime <= 100 && round (time / 8000) == List.length model.proton && time > 200 then
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
                --this level lost
                ( 0, 2 )

            else if List.length nproton <= 0 then
                --win
                ( 1, 1 )

            else
                ( -99, -99 )

        nmodel =
            reinitModel (sbm.level + add) model

        nsbm =
            nmodel.submodel
    in
    { nmodel | submodel = { nsbm | text_num = changeIndexToNewOne model.submodel.text_num trigue } }


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

            oldscore =
                model.submodel.score
        in
        case level of
            1 ->
                { prototype | size = prosize, submodel = { prosbm | state = Paused, text_num = 0 } }

            2 ->
                { prototype
                    | submodel = { prosbm | level = 2, state = Paused, text_num = 11 }
                    , spacecraft = addSpacecraft model.spacecraft
                    , proton = prototype.proton
                    , earth = { proEarth | pos = Point 700.0 500.0, show = Still, velocity = 0, radius = 0 }
                    , size = prosize
                }

            3 ->
                { prototype
                    | submodel = { prosbm | level = 3, state = Paused, text_num = 21 }
                    , spacecraft = addSpacecraft model.spacecraft
                    , proton = prototype.proton
                    , earth = { proEarth | pos = Point 700.0 700.0, show = Move, velocity = 0.01, radius = 200.0 }
                    , size = prosize
                }

            4 ->
                { prototype
                    | submodel = { prosbm | level = 4, state = Paused, text_num = 31 }
                    , spacecraft = addSpacecraft model.spacecraft
                    , proton = prototype.proton
                    , earth = { proEarth | pos = Point 700.0 700.0, show = Move, velocity = 0.01, radius = 200.0 }
                    , size = prosize
                }

            5 ->
                { model
                    | submodel = { prosbm | level = 4, state = Playing, score = oldscore, text_num = 31 }
                    , proton = prototype.proton
                }

            _ ->
                prototype


checkfailed : Model -> Model
checkfailed model =
    if model.submodel.heart <= 0 then
        model

    else
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
            { osbm | heart = old_heart - 1, state = Paused, text_num = changeIndexToNewOne osbm.text_num 4 }
    in
    if dis >= tracradius + 40 then
        { model | proton = nproton, seed = nseed, submodel = newSubmodel }

    else
        model


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


checkoutearth :
    Model
    -> Model
checkoutearth model =
    List.foldr checkoutearthInside model model.proton


checkoutearthInside :
    Proton
    -> Model
    -> Model --?
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

        opr =
            model.proton |> List.head |> Maybe.withDefault (Proton (Point 300 300) 0.6 7.5 2.0 6)

        heart_ =
            sbm.heart - 1

        ( nproton, nseed ) =
            reinitProton model.seed opr.intensity model.proton
    in
    { model
        | submodel = { sbm | heart = heart_, state = Paused, text_num = changeIndexToNewOne sbm.text_num 3 }
        , proton = nproton
        , seed = nseed
    }


protonbounce : Model -> Model
protonbounce model =
    model
        |> checkoutsun
        |> checkoutspc


checkoutsun : Model -> Model
checkoutsun model =
    let
        olddir =
            getdirfromproton model.proton
    in
    { model | proton = List.map (checkoutsunInside model) model.proton }
        |> changescore olddir


changescore : List Float -> Model -> Model
changescore list model =
    let
        submodel_ =
            model.submodel

        deltascore =
            checkoutbounce list (getdirfromproton model.proton)
    in
    { model | submodel = { submodel_ | score = model.submodel.score + 10 * deltascore } }


checkoutbounce : List Float -> List Float -> Int
checkoutbounce olddir newdir =
    let
        newlist =
            List.map2 (-) olddir newdir
    in
    List.filter (\x -> x /= 0) newlist |> List.length


getdirfromproton : List Proton -> List Float
getdirfromproton list =
    List.map getdir list


getdir : Proton -> Float
getdir proton =
    proton.dir


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

        d1 =
            dotLineDistance proton.pos a1 b1 c1

        d2 =
            dotLineDistance proton.pos a2 b2 c2
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
        Key_right _ ->
            angle - velocity

        Key_left _ ->
            angle + velocity

        Key_none _ ->
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
            x :: _ ->
                let
                    old =
                        List.drop 1 list |> List.head |> Maybe.withDefault defaultSpacecraft
                in
                x :: ({ old | dir = keydir } |> List.singleton)

            _ ->
                list


reinitProton : Random.Seed -> Int -> List Proton -> ( List Proton, Random.Seed )
reinitProton seed inten list =
    case list of
        _ :: xs ->
            let
                ( proton, nseed ) =
                    generateListOfProton seed (List.length list)

                nproton = List.map (\x -> {x | intensity = inten}) proton
            in
            ( nproton, nseed )

        _ ->
            ( list, seed )

generateListOfProton : Random.Seed -> Int -> ( List Proton, Random.Seed )
generateListOfProton seed num =
    let
        (p1,s1) = generateNewProton seed
        (p2,s2) = generateNewProton s1
        (p3,s3) = generateNewProton s2
        (p4,s4) = generateNewProton s3
        (p5,s5) = generateNewProton s4
    in
        case num of
            1 ->
                ([p1], s1)
            2 ->
                ([p1, p2], s2)
            3 ->
                ([p1, p2, p3], s3)
            4 ->
                ([p1, p2, p3, p4], s4)
            5 ->
                ([p1, p2, p3, p4, p5], s5)
            _ ->
                ([p1], s5)


generateNewProton : Random.Seed -> ( Proton, Random.Seed )
generateNewProton seed =
    let
        ( index, nseed ) =
            Random.step (Random.int 1 7) seed

        protonBox =
            [ Proton (Point 300 300) 0.2 7.5 2.0 5
            , Proton (Point 300 500) -1 10 2.0 8
            , Proton (Point 400 200) 1.5 8 1.5 5
            , Proton (Point 700 700) -1.9 8.5 1.5 5
            , Proton (Point 400 300) 1.4 10 2.0 5
            , Proton (Point 500 600) -1.0 10 2.0 5
            , Proton (Point 600 200) 1 10 2.0 5
            ]

        nproton =
            List.take index protonBox |> List.reverse |> List.head |> Maybe.withDefault (Proton (Point 300 500) -1 10 2.0 8)
    in
    ( nproton, nseed )


addSpacecraft : List Spacecraft -> List Spacecraft
addSpacecraft spacecraft =
    if List.length spacecraft == 1 then
        spacecraft ++ List.singleton (Spacecraft (Point 800.0 500.0) 0.0 (Key_none 2) 0.015)

    else
        spacecraft
