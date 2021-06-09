module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Browser.Navigation exposing (Key)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type Dir
    = Left
    | Right
    | None


type Moveable
    = Lost
    | Bounce


type Live_status
    = Dead
    | Alive


type alias Brick =
    { posx : Int
    , posy : Int
    }


type alias Paddle =
    { posx : Float
    , dir : Dir
    }


type alias Ball =
    { posx : Float
    , posy : Float
    , dir : Float
    , radius : Float
    }



--Model


type alias Model =
    { brick : List Brick
    , move_timer : Float
    , paddle : Paddle
    , ball : Ball
    }


brick_width =
    100


brick_height =
    25


screen_width =
    1000


screen_height =
    600


ball_radius =
    10



--Msg


type Msg
    = Key Dir
    | Tick Float



--Main


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--Initialization


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    Model (generatebricks ( 10, 5 )) 0 (Paddle 50.0 None) (Ball 100.0 400.0 45.0 10)


generateonebrick : ( Int, Int ) -> Brick
generateonebrick position =
    Brick (brick_width * Tuple.first position + 2) (brick_height * Tuple.second position + 2)


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
        |> List.map generateonebrick



--update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatepaddle msg


changepaddleDir : Dir -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
changepaddleDir keydir ( model, cmd ) =
    let
        dir1 =
            case keydir of
                Right ->
                    Right

                Left ->
                    Left

                None ->
                    None

        paddle1 =
            Paddle model.paddle.posx dir1
    in
    ( { model | paddle = paddle1 }
    , cmd
    )


updatepaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatepaddle msg ( model, cmd ) =
    case msg of
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
                |> paddlemove
                |> ballbounce
                |> ballball
            , cmd
            )

        Key keydir ->
            changepaddleDir keydir
                ( model
                    |> ballball
                    |> ballbounce
                    |> ballball
                , cmd
                )


paddlemove : Model -> Model
paddlemove model =
    let
        newx =
            paddlePosx model.paddle.posx model.paddle.dir
    in
    { model | paddle = makenewpaddle newx }


ballball : Model -> Model
ballball model =
    let
        newball =
            ballmove model.ball
    in
    { model | ball = newball }


ballmove : Ball -> Ball
ballmove ball =
    Ball (ball.posx + (cos (degrees ball.dir) * 0.5)) (ball.posy + (sin (degrees ball.dir) * 0.5)) ball.dir 10


changeballdir : Ball -> Float -> Ball
changeballdir ball number =
    Ball ball.posx ball.posy (number - ball.dir) 10


ballbounce : Model -> Model
ballbounce model =
    if model.ball.posy >= screen_height - model.ball.radius then
        { model | ball = changeballdir model.ball 360.0 }

    else if model.ball.posx >= screen_width - model.ball.radius then
        { model | ball = changeballdir model.ball 540.0 }

    else if model.ball.posx <= 0 + model.ball.radius then
        { model | ball = changeballdir model.ball 180.0 }

    else if model.ball.posy <= 0 + model.ball.radius then
        { model | ball = changeballdir model.ball 360.0 }

    else
        model


paddlePosx : Float -> Dir -> Float
paddlePosx oldx direction =
    case direction of
        Right ->
            oldx + 8.0

        Left ->
            oldx - 8.0

        None ->
            oldx


makenewpaddle : Float -> Paddle
makenewpaddle posx =
    Paddle posx None


drawPaddle : Paddle -> Svg msg
drawPaddle paddle =
    Svg.rect
        [ SvgAttr.width "100"
        , SvgAttr.height "20"
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString paddle.posx)
        , SvgAttr.y "400"
        ]
        []


drawBrick : Brick -> Svg Msg
drawBrick brick =
    Svg.rect
        [ SvgAttr.width (toString (brick_width - 2))
        , SvgAttr.height (toString (brick_height - 2))
        , SvgAttr.fill "Green"
        , SvgAttr.x (toString brick.posx)
        , SvgAttr.y (toString brick.posy)
        ]
        []


drawBricks : List Brick -> List (Svg Msg)
drawBricks list =
    List.map drawBrick list


drawball : Ball -> Svg Msg
drawball ball =
    Svg.circle
        [ SvgAttr.cx (toString ball.posx)
        , SvgAttr.cy (toString ball.posy)
        , SvgAttr.r (toString ball.radius)
        , SvgAttr.color "Red"
        ]
        []


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width (toString screen_width)
            , SvgAttr.height (toString screen_height)
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
            Key Left

        39 ->
            Key Right

        _ ->
            Key None
