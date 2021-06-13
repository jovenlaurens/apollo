module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Browser.Navigation exposing (Key)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Messages exposing (Keydir(..), Msg(..))
import Model exposing (Model, initial)
import Update exposing (update)
import View exposing (view)


main =
    Browser.element
        { init =
            \value ->
                ( value
                    |> Decode.decodeValue Model.decode
                    |> Result.withDefault Model.initial
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == Model.Playing then
            onAnimationFrameDelta Tick

          else
            Sub.none
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
    Key Key_none
