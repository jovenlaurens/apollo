module Model exposing (..)

--Model


type Dir
    = Left
    | Right
    | None


type Moveable
    = Lost
    | Bounce


type Live_status
    = Dead
    | Alive


type alias Brick =
    { posx : Int
    , posy : Int
    }


type alias Paddle =
    { posx : Float
    , dir : Dir
    }


type alias Ball =
    { posx : Float
    , posy : Float
    , dir : Float
    , radius : Float
    }


type alias Model =
    { brick : List Brick
    , move_timer : Float
    , paddle : Paddle
    , ball : Ball
    }


brick_width =
    100


brick_height =
    25


screen_width =
    1000


screen_height =
    600


ball_radius =
    10
