module Star exposing (..)
import Messages exposing (Keydir)
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
    , velocity : Float
    }

type alias Point =
    { x : Float
    , y : Float
    }
tracradius : Float
tracradius = 300.0

spcwidth : Float 
spcwidth = 30.0

spcheight : Float
spcheight = 10.0