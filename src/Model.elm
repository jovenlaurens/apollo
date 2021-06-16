module Model exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))
import Random
import Star exposing (Earth, Point, Proton, Spacecraft, Sun, sunRadius)


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
    Model (Sun (Point 500 500) sunRadius)
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
        ]


decodeSun : Decode.Decoder Sun
decodeSun =
    Decode.map2 Sun
        (Decode.field "sunp" decodePoint)
        (Decode.field "sunr" Decode.float)


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
