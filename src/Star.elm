module Star exposing (..)

import Point exposing (Point)


type alias Earth =
    { pos : Point
    , velocity : Float
    , lives : Int
    , radius : Float
    }


type alias Sun =
    { pos : Point
    , radius : Float
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
    , polar_axis : Float
    , polar_angle : Float
    , dir : Float --delta angle
    }
