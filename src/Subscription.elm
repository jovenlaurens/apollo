module Subscription exposing (..)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Browser.Navigation exposing (Key)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Messages exposing (..)
import Model exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown (Decode.map key keyCode)
        ]


key : Int -> Msg
key keycode =
    case keycode of
        37 ->
            Key Left

        39 ->
            Key Right

        _ ->
            Key None
