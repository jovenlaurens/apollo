module Model exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Messages exposing (Keydir(..), Msg(..))
import Star exposing (Earth, Point, Proton, Spacecraft, Sun)


type alias Model =
    { sun : Sun
    , earth : Earth
    , proton : Proton
    , spacecraft : Spacecraft
    , move_timer : Float
    , level : Int
    , state : State
    }


initial : Model
initial =
    Model (Sun (Point 500 500) 60.0)
        (Earth (Point 1 1) 0 0 0)
        (Proton (Point 50 350) 0.2 5.0 2.0 1)
        (Spacecraft (Point 800.0 500.0) 0.0 Key_none 0.01)
        0
        1
        Playing


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
        Key_left ->
            "keyleft"

        Key_right ->
            "keyright"

        _ ->
            "keynone"


decodeKeydir : String -> Keydir
decodeKeydir string =
    case string of
        "keyleft" ->
            Key_left

        "keyright" ->
            Key_right

        _ ->
            Key_none


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


encodeSpc : Spacecraft -> Encode.Value
encodeSpc spc =
    Encode.object
        [ ( "spcp", encodePoint spc.pos )
        , ( "spcv", Encode.float spc.velocity )
        , ( "spca", Encode.float spc.angle )
        , ( "spcd", Encode.string (encodeKeydir spc.dir) )
        ]


decodeSpc : Decode.Decoder Spacecraft
decodeSpc =
    Decode.map4 Spacecraft
        (Decode.field "spcp" decodePoint)
        (Decode.field "spca" Decode.float)
        (Decode.field "spcd" (Decode.map decodeKeydir Decode.string))
        (Decode.field "spcv" Decode.float)


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
