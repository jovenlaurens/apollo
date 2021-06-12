module Model exposing ( Model, State(..), init)
import Star exposing (Earth, Proton, Sun, Spacecraft, Point)
import Messages exposing (Keydir(..), Msg(..))


type alias Model =
    { sun : Sun
    , earth : Earth
    , proton : Proton
    , spacecraft : Spacecraft
    , move_timer : Float
    , level : Int --4 in all?
    }


type State
    = Playing
    | Stopped --either user pause or dead


init : () -> ( Model, Cmd Msg )
init a =
    ((Model (Sun (Point 500 500) 60.0) (Earth (Point 1 1) 0 0 0) 
            (Proton (Point 50 350) 0.2 5.0 2.0 1) (Spacecraft (Point 800.0 500.0) 0.0 Key_none 0.01)
            0 1), Cmd.none)


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
