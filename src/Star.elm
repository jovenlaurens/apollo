module Star exposing (Earth, Proton, Spacecraft, Sun, defaultSpacecraft)

import Geometry exposing (Point)
import Messages exposing (Earth_State(..), Keydir(..), Msg(..))


type alias Earth =
    { pos : Point
    , velocity : Float
    , radius : Float
    , angle : Float
    , show : Earth_State
    }


type alias Sun =
    { pos : Point
    , radius : Float
    , angle : Float
    }


type alias Proton =
    { pos : Point
    , dir : Float
    , radius : Float
    , velocity : Float
    , intensity : Int
    }


type alias Spacecraft =
    { pos : Point
    , angle : Float
    , dir : Keydir
    , velocity : Float --delta angle
    }


defaultSpacecraft =
    Spacecraft (Point 800.0 500.0) 0.0 (Key_none 1) 0.01
