module Star exposing (..)

import Messages exposing (Keydir)


type alias Point =
    { x : Float
    , y : Float
    }


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
    , angle : Float
    , dir : Keydir
    , velocity : Float --delta angle
    }


tracradius : Float
tracradius =
    300.0


spcwidth : Float
spcwidth =
    60.0


spcheight : Float
spcheight =
    20.0


originX =
    500.0


originY =
    500.0


sunRadius =
    60.0


availableScale =
    atan ((0.5 * spcwidth) / tracradius)