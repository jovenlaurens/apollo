module Messages exposing (Msg(..))


type Keydir
    = Key_right
    | Key_left
    | Key_none


type Msg
    = Start
    | Tick Float
    | Key Keydir
    | Pass Int --when level up
