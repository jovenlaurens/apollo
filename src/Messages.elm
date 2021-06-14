module Messages exposing (Keydir(..), Msg(..))


type Keydir
    = Key_right Int
    | Key_left Int
    | Key_none Int


type Msg
    = Start
    | Pause
    | Resume
    | Tick Float
    | Key Keydir
    | Reinit Int
