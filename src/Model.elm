module Model exposing (Model, State(..), SubModel, decode, encode, getHeadProton, initial)

import Geometry exposing (Point, availableScale, distance, dotLineDistance, getLine, originX, originY, sunRadius, tracradius, xShift, yShift)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))
import Random exposing (..)
import Star exposing (Earth, Proton, Spacecraft, Sun)


seed0 : Random.Seed
seed0 =
    Random.initialSeed 10


type alias SubModel =
    { move_timer : Float
    , level : Int
    , state : State
    , heart : Int
    , text_num : Int
    , score : Int 
    }


type alias Model =
    { sun : Sun
    , earth : Earth
    , proton : List Proton
    , spacecraft : List Spacecraft
    , submodel : SubModel
    , seed : Random.Seed
    , size : ( Float, Float )
    }


initial : Model
initial =
    Model (Sun (Point originX originY) sunRadius 0)
        (Earth (Point 0 0) 0 0 (-pi / 2) Not_show)
        (List.singleton (Proton (Point 300 300) 0.6 7.5 2.0 6))
        (List.singleton (Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.015))
        (SubModel 0 1 Cover 3 0 0)
        seed0
        ( 0, 0 )


getHeadProton : List Proton -> Proton
getHeadProton list =
    List.head list
        |> Maybe.withDefault (Proton (Point 300 300) 0.6 7.5 2.0 3)


type State
    = Cover
    | Interval
    | Playing
    | Paused
    | Stopped --either user pause or dead


changeEarthState : String -> Earth_State
changeEarthState string =
    case string of
        "still" ->
            Still

        "move" ->
            Move

        "not_show" ->
            Not_show

        _ ->
            Not_show


changeEarthString : Earth_State -> String
changeEarthString sta =
    case sta of
        Still ->
            "still"

        Move ->
            "move"

        Not_show ->
            "not_show"


decodeState : String -> State
decodeState string =
    case string of
        "playing" ->
            Playing

        "Paused" ->
            Paused

        "cover" ->
            Cover

        "intercal" ->
            Interval

        _ ->
            Stopped


encodeState : State -> String
encodeState state =
    case state of

        Playing ->
            "playing"

        Stopped ->
            "stopped"

        Paused ->
            "paused"

        Cover ->
            "cover"

        Interval ->
            "interval"


encodeKeydir : Keydir -> String
encodeKeydir keydir =
    case keydir of
        Key_left 1 ->
            "keyleft_Inside"

        Key_right 1 ->
            "keyright_Inside"

        Key_left 2 ->
            "keyleft_Outside"

        Key_right 2 ->
            "keyright_Outside"

        _ ->
            "keynone"


decodeKeydir : String -> Keydir
decodeKeydir string =
    case string of
        "keyleft_Inside" ->
            Key_left 1

        "keyright_Inside" ->
            Key_right 1

        "keyleft_Outside" ->
            Key_left 2

        "keyright_Outside" ->
            Key_right 2

        "keynone" ->
            Key_none 1

        _ ->
            Key_none 1


encodePoint : Point -> Encode.Value
encodePoint point =
    Encode.object
        [ ( "x", Encode.float point.x ), ( "y", Encode.float point.y ) ]


decodePoint : Decode.Decoder Point
decodePoint =
    Decode.map2 Point
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


encodeSun : Sun -> Encode.Value
encodeSun sun =
    Encode.object
        [ ( "sunp", encodePoint sun.pos )
        , ( "sunr", Encode.float sun.radius )
        , ( "suna", Encode.float sun.angle )
        ]


decodeSun : Decode.Decoder Sun
decodeSun =
    Decode.map3 Sun
        (Decode.field "sunp" decodePoint)
        (Decode.field "sunr" Decode.float)
        (Decode.field "suna" Decode.float)


encodeEarth : Earth -> Encode.Value
encodeEarth earth =
    Encode.object
        [ ( "earthp", encodePoint earth.pos )
        , ( "earthv", Encode.float earth.velocity )
        , ( "earthr", Encode.float earth.radius )
        , ( "eartha", Encode.float earth.angle )
        , ( "earthb", Encode.string (changeEarthString earth.show) )
        ]


decodeEarth : Decode.Decoder Earth
decodeEarth =
    Decode.map5 Earth
        (Decode.field "earthp" decodePoint)
        (Decode.field "earthv" Decode.float)
        (Decode.field "earthr" Decode.float)
        (Decode.field "eartha" Decode.float)
        (Decode.field "earthb" (Decode.map changeEarthState Decode.string))


encodeProton : Proton -> Encode.Value
encodeProton proton =
    Encode.object
        [ ( "protonp", encodePoint proton.pos )
        , ( "protonv", Encode.float proton.velocity )
        , ( "protonr", Encode.float proton.radius )
        , ( "protoni", Encode.int proton.intensity )
        , ( "protond", Encode.float proton.dir )
        ]


decodeProton : Decode.Decoder (List Proton)
decodeProton =
    Decode.list
        (Decode.map5
            Proton
            (Decode.field "protonp" decodePoint)
            (Decode.field "protond" Decode.float)
            (Decode.field "protonr" Decode.float)
            (Decode.field "protonv" Decode.float)
            (Decode.field "protoni" Decode.int)
        )


encodeSpacecraft : Spacecraft -> Encode.Value
encodeSpacecraft spc =
    Encode.object
        [ ( "spcp", encodePoint spc.pos )
        , ( "spca", Encode.float spc.angle )
        , ( "spcd", Encode.string (encodeKeydir spc.dir) )
        , ( "spcv", Encode.float spc.velocity )
        ]


decodeSpc : Decode.Decoder (List Spacecraft)
decodeSpc =
    Decode.list
        (Decode.map4
            Spacecraft
            (Decode.field "spcp" decodePoint)
            (Decode.field "spca" Decode.float)
            (Decode.field "spcd" (Decode.map decodeKeydir Decode.string))
            (Decode.field "spcv" Decode.float)
        )


encodesubmodel : SubModel -> Encode.Value
encodesubmodel submodel =
    Encode.object
        [ ( "move_timer", Encode.float submodel.move_timer )
        , ( "level", Encode.int submodel.level )
        , ( "state", Encode.string (encodeState submodel.state) )
        , ( "heart", Encode.int submodel.heart )
        , ( "text", Encode.int submodel.text_num )
        , ( "score", Encode.int submodel.score )
        ]


decodesubmodel : Decode.Decoder SubModel
decodesubmodel =
    Decode.map6 SubModel
        (Decode.field "move_timer" Decode.float)
        (Decode.field "level" Decode.int)
        (Decode.field "state" (Decode.map decodeState Decode.string))
        (Decode.field "heart" Decode.int)
        (Decode.field "text" Decode.int)
        (Decode.field "score" Decode.int)


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "earth", encodeEarth model.earth )
            , ( "sun", encodeSun model.sun )
            , ( "spacecraft", Encode.list encodeSpacecraft model.spacecraft )
            , ( "proton", Encode.list encodeProton model.proton )
            , ( "submodel", encodesubmodel model.submodel )
            ]
        )


decode : Decode.Decoder Model
decode =
    Decode.map5
        (\earth sun spacecraft proton submodel ->
            { initial
                | earth = earth
                , sun = sun
                , spacecraft = spacecraft
                , proton = proton
                , submodel = submodel
            }
        )
        (Decode.field "earth" decodeEarth)
        (Decode.field "sun" decodeSun)
        (Decode.field "spacecraft" decodeSpc)
        (Decode.field "proton" decodeProton)
        (Decode.field "submodel" decodesubmodel)


defaultSpacecraft =
    Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.01


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


{-| the repo for new proton
-}
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


addSpacecraft : List Spacecraft -> List Spacecraft
addSpacecraft spacecraft =
    if List.length spacecraft == 1 then
        spacecraft ++ List.singleton (Spacecraft (Point 300.0 500.0) pi (Key_none 2) 0.015)

    else
        spacecraft


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


protonbounce : Model -> Model
protonbounce model =
    model
        |> checkoutsun
        |> checkoutspc


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


{-| anglebtwpoint : Point -> Point -> Float
anglebtwpoint pa pb =
atan2 (pb.y - pa.y) (pa.x - pb.x)
-}
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
