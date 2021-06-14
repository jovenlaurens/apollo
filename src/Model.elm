module Model exposing (..)

import Html.Attributes exposing (list)
import Json.Decode as Decode
import Json.Encode as Encode
import Messages exposing (Keydir(..), Msg(..))
import Star exposing (Earth, Point, Proton, Spacecraft, Sun)
import Tuple exposing (first, second)


type alias Model =
    { sun : Sun
    , earth : Earth
    , proton : Proton
    , spacecraft : List Spacecraft
    , move_timer : Float
    , level : Int
    , state : State
    , heart : Int
    }


initial : Model
initial =
    Model (Sun (Point 500 500) 60.0)
        (Earth (Point 1 1) 0 0 0)
        (Proton (Point 50 350) 0.2 7.5 2.0 5)
        (List.singleton (Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.01))
        0
        1
        Stopped
        2


defaultSpacecraft =
    Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.01


type State
    = Playing
    | Paused
    | Stopped --either user pause or dead


decodeState : String -> State
decodeState string =
    case string of
        "playing" ->
            Playing

        "Paused" ->
            Paused

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
        [ ( "earthx", Encode.float earth.pos.x )
        , ( "earthy", Encode.float earth.pos.y )
        , ( "earthv", Encode.float earth.velocity )
        , ( "earthr", Encode.float earth.radius )
        , ( "earthl", Encode.int earth.lives )
        ]


decodeEarth : Decode.Decoder Earth
decodeEarth =
    Decode.map4 Earth
        (Decode.field "earthp" decodePoint)
        (Decode.field "earthv" Decode.float)
        (Decode.field "earthl" Decode.int)
        (Decode.field "earthr" Decode.float)


encodeProton : Proton -> Encode.Value
encodeProton proton =
    Encode.object
        [ ( "protonp", encodePoint proton.pos )
        , ( "protonv", Encode.float proton.velocity )
        , ( "protonr", Encode.float proton.radius )
        , ( "protoni", Encode.int proton.intensity )
        , ( "protond", Encode.float proton.dir )
        ]


decodeProton : Decode.Decoder Proton
decodeProton =
    Decode.map5 Proton
        (Decode.field "protonp" decodePoint)
        (Decode.field "protond" Decode.float)
        (Decode.field "protonr" Decode.float)
        (Decode.field "protonv" Decode.float)
        (Decode.field "protoni" Decode.int)


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


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "earth", encodeEarth model.earth )
            , ( "sun", encodeSun model.sun )
            , ( "spacecraft", encodeSpc model.spacecraft )
            , ( "proton", encodeProton model.proton )
            , ( "level", Encode.int model.level )
            , ( "state", Encode.string (encodeState model.state) )
            ]
        )


decode : Decode.Decoder Model
decode =
    Decode.map6
        (\earth sun spacecraft proton level state ->
            { initial
                | earth = earth
                , sun = sun
                , spacecraft = spacecraft
                , proton = proton
                , level = level
                , state = state
            }
        )
        (Decode.field "earth" decodeEarth)
        (Decode.field "sun" decodeSun)
        (Decode.field "spacecraft" decodeSpc)
        (Decode.field "proton" decodeProton)
        (Decode.field "level" Decode.int)
        (Decode.field "state" (Decode.map decodeState Decode.string))
