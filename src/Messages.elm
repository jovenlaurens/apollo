module Messages exposing (Keydir(..), Msg(..), Earth_State(..))
import Browser.Dom exposing (Viewport)


type Keydir
    = Key_right Int
    | Key_left Int
    | Key_none Int


type Earth_State
    = Still
    | Move
    | Not_show

type Msg
    = Start
    | Pause
    | Resume
    | Tick Float
    | Key Keydir
    | Reinit Int
    | ChangeText Int Int
    | EnterGame
    | EnterCover
    | PlayInterval
    | GetViewport Viewport
    | Resize Int Int

