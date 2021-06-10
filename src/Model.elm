module Model exposing
    ( Model
    , State(..)
    )

import Point exposing (Point, move)
import Star exposing (Earth, Proton, Sun)


type alias Model =
    { sun : Sun
    , earth : Earth
    , move_timer : Float
    , level : Int --4 in all?

    --, proton : Proton
    }


type State
    = Playing
    | Stopped --either user pause or dead


init : Model
init =
    Model (Sun (Point 1 1) 0) (Earth (Point 1 1) 0 0 0) 0 1


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
