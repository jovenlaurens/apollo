module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Messages exposing (Keydir(..), Msg(..))
import Model exposing (Model)
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
        , onKeyDown (Decode.map (key model) keyCode)
        , onKeyUp (Decode.map (key_up model) keyCode)
        ]


key : Model -> Int -> Msg
key model keycode =
    case model.level of
        1 ->
            case keycode of
                37 ->
                    Key (Key_left 1)

                39 ->
                    Key (Key_right 1)

                _ ->
                    Key (Key_none 1)

        _ ->
            case keycode of
                37 ->
                    Key (Key_left 1)

                39 ->
                    Key (Key_right 1)

                65 ->
                    Key (Key_left 2)

                68 ->
                    Key (Key_right 2)

                --need to be improved
                _ ->
                    Key (Key_none 1)


key_up : Model -> Int -> Msg
key_up model keycode =
    case model.level of
        1 ->
            case keycode of
                37 ->
                    Key (Key_none 1)

                39 ->
                    Key (Key_none 1)

                _ ->
                    Key (Key_none 1)

        _ ->
            case keycode of
                37 ->
                    Key (Key_none 1)

                39 ->
                    Key (Key_none 1)

                65 ->
                    Key (Key_none 2)

                68 ->
                    Key (Key_none 2)

                --need to be improved
                _ ->
                    Key (Key_none 1)
