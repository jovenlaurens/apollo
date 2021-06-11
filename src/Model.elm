module Model exposing
    ( Model
    , State(..)
    , init
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Point exposing (Point, move)
import Random exposing (..)
import Star exposing (Earth, Proton, Sun)


type alias Model =
    { sun : Sun
    , earth : Earth
    , move_timer : Float
    , level : Int --4 in all?
    , seed : Random.Seed --used to decide the ball initial speed

    --, proton : Proton
    }


type State
    = Playing
    | Stopped --either user pause or dead


init : Model
init =
    Model (Sun (Point 1 1) 0) (Earth (Point 1 1) 0 0 0) 0 1 (Random.initialSeed 1)


decodeState : String -> State
decodeState string =
    case string of
        "playing" ->
            Playing

        _ ->
            Stopped


encodeState : State -> String
encodeState state =
    case state of
        Playing ->
            "playing"

        Stopped ->
            "stopped"


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "sunposx", Encode.float model.sun.pos.x )
            ]
        )


decode : Decode.Decoder String
decode =
    Decode.field "sunposx" Decode.string
