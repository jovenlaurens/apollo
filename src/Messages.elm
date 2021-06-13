module Messages exposing (Keydir(..), Msg(..))


type Keydir
    = Key_right
    | Key_left
    | Key_none


type Msg
    = Start
    | Pause
    | Resume
    | Tick Float
    | Key Keydir
    | Pass Int --when level up
