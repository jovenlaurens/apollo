module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Browser.Navigation exposing (Key)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias Point =
    { x : Float
    , y : Float
    }

type LocationOne
    = Left
    |Right
    |Middle
type alias Location =
    {
  xlocation : LocationOne
  ,ylocation: LocationOne
    }

type Keydir
    = Key_right
    | Key_left
    | Key_none


type Live_status
    = Dead
    | Alive


type alias Brick =
    { pos : Point
    , half_length : Float
    , half_width : Float
    }


type alias Paddle =
    { pos : Point
    , half_length : Float
    , half_width : Float
    }


type alias Ball =
    { pos : Point
    , move_x : Float
    , move_y : Float
    , radius : Float
    }


type alias Model =
    { brick : List Brick
    , move_timer : Float
    , paddle : Paddle
    , ball : Ball
    , live_statue : Live_status
    }


type Msg
    = Key Keydir
    | Tick Float


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )


window_length =
    1000.0


window_width =
    1000.0


brick_number =
    ( 10, 8 )


brick_half_width =
    40


brick_half_length =
    50


initModel : Model
initModel =
    Model (generatebricks brick_number) 0 (Paddle (Point 500.0 700.0) Key_none (generateRectLines_4 (Point 500.0 700.0))) (Ball (Point 100.0 400.0) 45.0 Alive)


generatebricks : ( Int, Int ) -> List Brick
generatebricks size =
    let
        rangex =
            List.range 0 (Tuple.first size - 1)

        rangey =
            List.range 0 (Tuple.second size - 1)

        line =
            \y -> List.map (\x -> Tuple.pair x y) rangex
    in
    List.map line rangey
        |> List.concat
        |> List.map toFloatPoint
        |> List.map generateonebrick


generateonebrick : ( Float, Float ) -> Brick
generateonebrick position =
    let
        pos =
            Point (2 * brick_half_length * Tuple.first position + 1) (2 * brick_half_width * Tuple.second position - 1)
    in
    Brick pos 50 40


toFloatPoint : ( Int, Int ) -> ( Float, Float )
toFloatPoint ( x, y ) =
    ( Basics.toFloat x, Basics.toFloat y )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatepaddle msg


updatepaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatepaddle msg ( model, cmd ) =
    case msg of
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
                |> ballBounce
                |> ballMove
                --|> check_brick
            , cmd
            )

        Key keydir ->
            ( model
                |> ballBounce
                |> ballMove
                |> paddleMove keydir
            , cmd
            )


ballBounce : Model -> Model
ballBounce model =
    ballWindow model
        |> ballBrick


ballWindow : Model -> Model
ballWindow model =
    if model.ball.pos.x <= model.ball.radius || model.ball.pos.x >= window_length - model.ball.radius then
        { model | ball = { pos = model.ball.pos, move_x = -model.ball.move_x, move_y = model.ball.move_y, radius = model.ball.radius } }

    else if model.ball.pos.y <= model.ball.radius || model.ball.pos.y >= window_width - model.ball.radius then
        { model | ball = { pos = model.ball.pos, move_x = model.ball.move_x, move_y = -model.ball.move_y, radius = model.ball.radius } }

    else
        model

ballBrick : Model->Model
ballBrick model =


ballMove : Model -> Model
ballMove model =
    { model | ball = { pos = helpBallMove model.ball.move_x model.ball.move_y model.ball.pos, move_x = model.ball.move_x, move_y = model.ball.move_y, radius = model.ball.radius } }

getCloseClass: Model ->(Point,Location)
getCloseClass model =


paddleMove : Keydir -> Model -> Model
paddleMove keydir model =
    { model
        | paddle =
            { pos = helpPaddleMove keydir model.paddle.pos
            , half_length = model.paddle.half_length
            , half_width = model.paddle.half_width
            }
    }


helpBallMove : Float -> Float -> Point -> Point
helpBallMove move_x move_y old_point =
    Point (old_point.x + move_x) (old_point.y + move_y)


helpPaddleMove : Keydir -> Point -> Point
helpPaddleMove keydir point =
    case keydir of
        Key_left ->
            Point (point.x - 5.0) point.y

        Key_right ->
            Point (point.x + 5.0) point.y

        _ ->
            point


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
            Key Key_left

        39 ->
            Key Key_right

        _ ->
            Key Key_none
