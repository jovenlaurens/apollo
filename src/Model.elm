module Model exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))
import Random
import Star exposing (Earth, Point, Proton, Spacecraft, Sun, sunRadius, sunRotateSpeed, originX, originY)


seed0 : Random.Seed
seed0 =
    Random.initialSeed 10



--need to be improved


type alias SubModel =
    { move_timer : Float
    , level : Int
    , state : State
    , heart : Int
    , text_num : Int
    , score : Int --无尽模式下的分数
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
        (List.singleton (Proton (Point 300 300) 0.6 7.5 2.0 8))
        (List.singleton (Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.01))
        (SubModel 0 1 BeforePlay 3 0 0)
        seed0
        ( 0, 0 )


defaultSpacecraft =
    Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.01


getHeadProton : List Proton -> Proton
getHeadProton list =
    List.head list
        |> Maybe.withDefault (Proton (Point 300 300) 0.6 7.5 2.0 3)


type State
    = BeforePlay
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

        "beforeplay" ->
            BeforePlay

        _ ->
            Stopped


encodeState : State -> String
encodeState state =
    case state of
        BeforePlay ->
            "beforeplay"

        Playing ->
            "playing"

        Stopped ->
            "stopped"

        Paused ->
            "paused"


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
        , ( "suna", Encode.float sun.angle)
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


encodeSpc : List Spacecraft -> Encode.Value
encodeSpc spc =
    let
        spc1 =
            List.head spc |> Maybe.withDefault defaultSpacecraft

        spc2 =
            List.reverse spc |> List.head |> Maybe.withDefault defaultSpacecraft
    in
    Encode.object
        [ ( "spcp1", encodePoint spc1.pos )
        , ( "spcv1", Encode.float spc1.velocity )
        , ( "spca1", Encode.float spc1.angle )
        , ( "spcd1", Encode.string (encodeKeydir spc1.dir) )
        , ( "spcp2", encodePoint spc2.pos )
        , ( "spcv2", Encode.float spc2.velocity )
        , ( "spca2", Encode.float spc2.angle )
        , ( "spcd2", Encode.string (encodeKeydir spc2.dir) )
        ]


decodeSpc : Decode.Decoder (List Spacecraft)
decodeSpc =
    Decode.map8 decodeListSpc
        (Decode.field "spcp1" decodePoint)
        (Decode.field "spcv1" Decode.float)
        (Decode.field "spcd1" (Decode.map decodeKeydir Decode.string))
        (Decode.field "spca1" Decode.float)
        (Decode.field "spcp2" decodePoint)
        (Decode.field "spcv2" Decode.float)
        (Decode.field "spcd2" (Decode.map decodeKeydir Decode.string))
        (Decode.field "spca2" Decode.float)


decodeListSpc a b c d e f g h =
    let
        first =
            Spacecraft a b c d

        second =
            Spacecraft e f g h
    in
    if d == h && b == f then
        List.singleton first

    else
        first :: List.singleton second


encodesubmodel : SubModel -> Encode.Value
encodesubmodel submodel =
    Encode.object
        [ ("move_timer", Encode.float submodel.move_timer)
        , ("level", Encode.int submodel.level)
        , ("state", Encode.string (encodeState submodel.state))
        , ("heart", Encode.int submodel.heart)
        , ("text", Encode.int submodel.text_num)
        , ("score", Encode.int submodel.score)
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
            , ( "spacecraft", encodeSpc model.spacecraft )
            , ( "proton", Encode.list encodeProton model.proton )
            , ( "submodel", encodesubmodel model.submodel)
            ]
        )


decode : Decode.Decoder Model
decode =
    Decode.map5
        (\earth sun spacecraft proton submodel->
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
        
