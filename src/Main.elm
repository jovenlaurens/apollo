module Main exposing (main)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Browser.Navigation exposing (Key)
import Json.Decode as Decode
import Browser.Events exposing (onKeyUp)


type Keydir
    = Key_right
    | Key_left
    | Key_none

type Moveable 
        = Lost
        | Bounce

type Live_status
        = Dead
        | Alive

type alias Brick
        = {
            posx : Int
            ,posy : Int
            
        }

type alias Paddle
        = {posx : Float
        , dir : Keydir
        }

type alias Ball
        ={
            posx : Float
            ,posy : Float
            ,dir : Float
        }

--Model
type alias  Model 
        = { 
            brick : List Brick
            , move_timer : Float
            , paddle : Paddle
            , ball : Ball
        }

--Msg
type Msg
    = Key Keydir
    | Tick Float

--Main
main = 
        Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

--Initialization

brick_number = ( 10, 5 )
init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )
    
initModel : Model
initModel =
        Model (generatebricks brick_number) 0 (Paddle  50.0 Key_none) (Ball 100.0 400.0 45.0 )

generateonebrick : ( Int, Int ) -> Brick
generateonebrick position =
        Brick (150 * Tuple.first position) (20 * Tuple.second position)
 
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


updatepaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatepaddle msg ( model, cmd ) = 
    case msg of
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
                |> ballbounce
                |> ballmove
                |> paddlemove
            , cmd
            )

        Key keydir ->
            (model
                |> ballbounce
                |> ballmove
                |> paddlechange keydir
            ,cmd)

paddlechange : Keydir -> Model -> Model
paddlechange keydir model = 
    
    {model | paddle = {posx = model.paddle.posx, dir = keydir}}

paddlemove : Model -> Model
paddlemove model = 
        let
            newx = paddlePosx model.paddle.posx model.paddle.dir
        in
            {model | paddle = { posx = newx,dir = model.paddle.dir }}



ballmove : Model -> Model
ballmove model =
     {model| ball = (Ball (model.ball.posx + ((cos (degrees model.ball.dir)) * 2)) (model.ball.posy + ((sin (degrees model.ball.dir)) * 2)) model.ball.dir)}

changeballdir : Ball -> Float -> Ball
changeballdir ball number = 
        Ball ball.posx ball.posy (number - ball.dir)

ballbounce : Model -> Model
ballbounce model = 
        if ((model.ball.posy >= 693.5) && (model.ball.posy <= 696.5) && (model.ball.posx <= (model.paddle.posx + 80.0)) && (model.ball.posx >= model.paddle.posx)) then
                {model | ball = changeballdir model.ball 360.0 }

        else if ((model.ball.posx >= 1500.5) && ( model.ball.posx <= 1505.5 )) then
                {model | ball = changeballdir model.ball 540.0 }

        else if ((model.ball.posx >= 0.0) && ( model.ball.posx <= 6.5 )) then
                {model | ball = changeballdir model.ball 180.0 }

        else if ((model.ball.posy >= 97.5) && ( model.ball.posy <= 102.5 )) then
                {model | ball = changeballdir model.ball 360.0 }
        else 
            model
paddlePosx : Float  -> Keydir -> Float
paddlePosx oldx direction = 
    case direction of
        Key_right ->
             oldx + 5.0

        Key_left ->
             oldx - 5.0
        
        Key_none ->
             oldx

--makenewpaddle : Float ->Paddle
--makenewpaddle posx =
    --Paddle posx None--感觉这么操作的话会导致掉帧（bushi

drawPaddle : Paddle -> Svg msg
drawPaddle paddle =
    Svg.rect    
        [ SvgAttr.width "80"
        , SvgAttr.height "10"
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString paddle.posx)
        , SvgAttr.y "700"
        ]
        []

drawBrick : Brick -> Svg Msg
drawBrick brick =
    Svg.rect[
                 SvgAttr.width "148"
                , SvgAttr.height "18"
                , SvgAttr.fill "Green"
                , SvgAttr.x (toString (brick.posx))
                , SvgAttr.y (toString (brick.posy))
    ][]

drawBricks : List Brick -> List (Svg Msg)
drawBricks list = 
        List.map drawBrick list

drawball : Ball -> Svg Msg
drawball ball =
        Svg.circle[
                    SvgAttr.cx (toString ball.posx)
                    ,SvgAttr.cy (toString ball.posy)
                    ,SvgAttr.r "5"
                    ,SvgAttr.color "Red"
                    ][]

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
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ] (drawball model.ball :: ((drawPaddle model.paddle) :: (drawBricks model.brick)))
        ]

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