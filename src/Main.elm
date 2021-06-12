module Main exposing (main)
import Update
import View
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Messages exposing (Msg(..), Keydir(..))
import Model exposing (Model, init)

main =
    Browser.element
        { init = Model.init
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown (Decode.map key keyCode)
        , onKeyUp (Decode.map key_up keyCode)
        ]


key : Int -> Msg
key keycode =
    case keycode of
        37 ->
            Key Key_left

        39 ->
            Key Key_right

        _ ->
            Key Key_none


key_up : Int -> Msg
key_up keycode =
    case keycode of
        37 ->
            Key Key_none

        39 ->
            Key Key_none

        _ ->
            Key Key_none
