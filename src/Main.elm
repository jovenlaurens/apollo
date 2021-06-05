module Main exposing (..)

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
    | Right
    | Middle


type alias Location =
    { xlocation : LocationOne
    , ylocation : LocationOne
    }


type Keydir
    = Key_right
    | Key_left
    | Key_none


type Live_status
    = Dead
    | Alive


type Exist_status
    = Exist
    | Disappear


type alias Brick =
    { pos : Point --center point
    , half_length : Float
    , half_width : Float
    , exist : Exist_status
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
    800.0


brick_number =
    ( 10, 8 )


brick_half_width =
    12.0


brick_half_length =
    42.0


paddle_half_length =
    70.0


paddle_half_width =
    8.0


ball_radius =
    15.0


initModel : Model
initModel =
    Model (generatebricks brick_number) 0 (Paddle (Point 700.0 750.0) paddle_half_length paddle_half_width) (Ball (Point 100.0 400.0) -2.0 -2.0 ball_radius) Alive


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
            Point ((brick_half_length + 8) * (2 * Tuple.first position + 1)) ((brick_half_width + 8) * (2 * Tuple.second position + 1))
    in
    Brick pos brick_half_length brick_half_width Exist


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
                |> ballWindow
                |> ballBounce
                |> ballMove
            , cmd
            )

        Key keydir ->
            ( model
                |> ballWindow
                |> ballBounce
                |> ballMove
                |> paddleMove keydir
            , cmd
            )


ballBounce : Model -> Model
ballBounce model =
    model
        |> getCloseClass
        |> ballChangeMovePaddle model
        |> ballBrick
        |> clearBrick


ballWindow : Model -> Model
ballWindow model =
    if (model.ball.pos.x <= model.ball.radius) || (model.ball.pos.x >= window_length - model.ball.radius) then
        { model | ball = { pos = model.ball.pos, move_x = -model.ball.move_x, move_y = model.ball.move_y, radius = model.ball.radius } }

    else if model.ball.pos.y <= model.ball.radius || model.ball.pos.y >= window_width - model.ball.radius then
        { model | ball = { pos = model.ball.pos, move_x = model.ball.move_x, move_y = -model.ball.move_y, radius = model.ball.radius } }

    else
        model


getCloseClassBrick : Ball -> Brick -> ( Location, Float )
getCloseClassBrick ball brick =
    let
        ( close_class_x, pointx ) =
            if ball.pos.x < (brick.pos.x - brick.half_length) then
                ( Left, brick.pos.x - brick.half_length )

            else if ball.pos.x > (brick.pos.x + brick.half_length) then
                ( Right, brick.pos.x + brick.half_length )

            else
                ( Middle, brick.pos.x )

        ( close_class_y, pointy ) =
            if ball.pos.y < (brick.pos.y - brick.half_width) then
                ( Left, brick.pos.y - brick.half_width )

            else if ball.pos.y > (brick.pos.y + brick.half_width) then
                ( Right, brick.pos.y + brick.half_width )

            else
                ( Middle, brick.pos.y )
    in
    ( Location close_class_x close_class_y, sqrt ((pointx - ball.pos.x) ^ 2 + (pointy - ball.pos.y) ^ 2) )


ballChangeMoveBrick : ( Location, Float ) -> Ball -> Ball
ballChangeMoveBrick ( closeClass, distance ) ball =
    if distance < ball.radius && (closeClass.ylocation == Left || closeClass.ylocation == Right) && (closeClass.xlocation == Left || closeClass.xlocation == Right) then
        if closeClass.xlocation == Left && ball.move_x > 0 then
            { ball | move_x = -ball.move_x, move_y = -ball.move_y }

        else if closeClass.xlocation == Left && ball.move_x < 0 then
            { ball | move_y = -ball.move_y }

        else if closeClass.xlocation == Right && ball.move_x < 0 then
            { ball | move_x = -ball.move_x, move_y = -ball.move_y }

        else
            { ball | move_y = -ball.move_y }

    else if distance < ball.radius && closeClass.xlocation == Middle then
        { ball | move_y = -ball.move_y }

    else
        ball


brickChangeState : Ball -> Brick -> ( Location, Float ) -> Brick
brickChangeState ball brick ( closeClass, distance ) =
    if distance < ball.radius && (closeClass.ylocation == Left || closeClass.ylocation == Right) && (closeClass.xlocation == Left || closeClass.xlocation == Right) then
        if closeClass.xlocation == Left && ball.move_x > 0 then
            { brick | exist = Disappear }

        else if closeClass.xlocation == Left && ball.move_x < 0 then
            { brick | exist = Disappear }

        else if closeClass.xlocation == Right && ball.move_x < 0 then
            { brick | exist = Disappear }

        else
            { brick | exist = Disappear }

    else if distance < ball.radius && closeClass.xlocation == Middle then
        { brick | exist = Disappear }

    else
        brick


clearBrick : Model -> Model
clearBrick model =
    let
        helpClearBrick : Brick -> Bool
        helpClearBrick brick =
            brick.exist == Exist
    in
    { model | brick = List.filter helpClearBrick model.brick }


ballBrick : Model -> Model
ballBrick model =
    let
        nball =
            List.map (getCloseClassBrick model.ball) model.brick
                |> List.foldl ballChangeMoveBrick model.ball

        nbrick =
            List.map (getCloseClassBrick model.ball) model.brick
                |> List.map2 (brickChangeState model.ball) model.brick
    in
    { model | ball = nball, brick = nbrick }


ballChangeMovePaddle : Model -> ( Location, Float ) -> Model
ballChangeMovePaddle model ( close_class, distance ) =
    if distance < 1.5 * model.ball.radius then
        { model | ball = { pos = model.ball.pos, move_x = model.ball.move_x, move_y = -model.ball.move_y, radius = model.ball.radius } }

    else if close_class.ylocation /= Left then
        { model | live_statue = Dead }

    else
        model


ballMove : Model -> Model
ballMove model =
    { model | ball = { pos = helpBallMove model.ball.move_x model.ball.move_y model.ball.pos, move_x = model.ball.move_x, move_y = model.ball.move_y, radius = model.ball.radius } }


getCloseClass : Model -> ( Location, Float )
getCloseClass model =
    let
        ( close_class_x, pointx ) =
            if model.ball.pos.x < (model.paddle.pos.x - model.paddle.half_length) then
                ( Left, model.paddle.pos.x - model.paddle.half_length )

            else if model.ball.pos.x > (model.paddle.pos.x + model.paddle.half_length) then
                ( Right, model.paddle.pos.x + model.paddle.half_length )

            else
                ( Middle, model.paddle.pos.x )

        ( close_class_y, pointy ) =
            if model.ball.pos.y < (model.paddle.pos.y - model.paddle.half_width) then
                ( Left, model.paddle.pos.y - model.paddle.half_width )

            else if model.ball.pos.y > (model.paddle.pos.y + model.paddle.half_width) then
                ( Right, model.paddle.pos.y + model.paddle.half_width )

            else
                ( Middle, model.paddle.pos.y )
    in
    ( Location close_class_x close_class_y, sqrt ((pointx - model.ball.pos.x) ^ 2 + (pointy - model.ball.pos.y) ^ 2) )


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
            Point (point.x - 15.0) point.y

        Key_right ->
            Point (point.x + 15.0) point.y

        _ ->
            point


drawPaddle : Paddle -> Svg msg
drawPaddle paddle =
    Svg.rect
        [ SvgAttr.width (toString (paddle.half_length * 2))
        , SvgAttr.height (toString (paddle.half_width * 2))
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString (paddle.pos.x - paddle.half_length))
        , SvgAttr.y (toString (paddle.pos.y + paddle.half_width))
        ]
        []


drawBrick : Brick -> Svg Msg
drawBrick brick =
    Svg.rect
        [ SvgAttr.width (toString (brick.half_length * 2))
        , SvgAttr.height (toString (brick.half_width * 2))
        , SvgAttr.fill "Green"
        , SvgAttr.x (toString (brick.pos.x - brick_half_length))
        , SvgAttr.y (toString (brick.pos.y + brick_half_width))
        ]
        []


drawBricks : List Brick -> List (Svg Msg)
drawBricks list =
    List.map drawBrick list


drawball : Ball -> Svg Msg
drawball ball =
    Svg.circle
        [ SvgAttr.cx (toString ball.pos.x)
        , SvgAttr.cy (toString ball.pos.y)
        , SvgAttr.r (toString ball.radius)
        , SvgAttr.color "Red"
        ]
        []


renderInfo : Live_status -> Html Msg
renderInfo state =
    div
        [ style "background" "rgba(75,0,130, 0.7)"
        , style "color" "#00FF00"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "50px"
        , style "font-weight" "bold"
        , style "line-height" "10"
        , style "position" "absolute"
        , style "top" "0"
        , style "width" "501px"
        , style "height" "501px"
        , style "display"
            (if state == Alive then
                "none"

             else
                "block"
            )
        ]
        [ text "You Died!"
        ]


fourBorders_String =
    "0 0 1000 1000"


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ renderInfo model.live_statue
        , Svg.svg
            [ SvgAttr.width "1000"
            , SvgAttr.height "800"
            , SvgAttr.viewBox fourBorders_String
            ]
            (drawball model.ball :: (drawPaddle model.paddle :: drawBricks model.brick))
        ]


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
