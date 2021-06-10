module Model exposing (Model)

type alias Point =
    { x : Float
    , y : Float
    }

type Keydir
    = Key_right
    | Key_left
    | Key_none

type Live_status
    = Dead
    | Alive

type alias Spacecraft =
    { pos : Point
    , dir : Keydir
    , velocity : Float
    }

type alias Sun = 
    { pos : Point
      , radius : Float
    }

--dir refers to the angle in the polar coordinate

type alias Proton =
    { pos : Point
    , dir : Float
    , radius : Float
    , velocity : Float
    , intensity : Int
    }

type alias Earth = 
    { pos : Point
    , status : Live_status
    , velocity : Float
    }
--Model
type alias Model =
    { 
    sun : Sun
    , move_timer : Float
    , spacecraft : Spacecraft
    , proton : Proton
    , earth : Earth
    , level : Int 
    }
