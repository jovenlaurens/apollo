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
import Svg.Attributes exposing (y1)


type alias Point =
    { x : Float
    , y : Float
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
    }


type alias Paddle =
    { pos : Point
    , dir : Keydir
    }


type alias Ball =
    { pos : Point
    , dir : Float
    , liveStatus : Live_status --need to be improved: whether it should be placed in the ball
    , radius : Float
    }



--Model


type alias Model =
    { brick : List Brick
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


brick_number =
    ( 10, 8 )


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    Model 
        (generatebricks brick_number) 0 
        (Paddle (Point 500.0 900.0) Key_none ) 
        (Ball (Point 100.0 600.0) 45.0 Alive ball_radius)


brick_width =
    100



--砖块的长


brick_height =
    50



--宽





generateonebrick : ( Float, Float ) -> Brick
generateonebrick position =
    let
        pos = Point (brick_width * Tuple.first position + 1) (brick_height * Tuple.second position - 1)
    in
    
    Brick pos 

toFloatPoint : (Int, Int) -> (Float, Float)
toFloatPoint (x, y) = 
    (Basics.toFloat x,Basics.toFloat y)

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
                |> ballmove
                |> paddlemove
                |> ballbounce
            , cmd
            )

        Key keydir ->
            ( model
                |> ballmove
                |> paddlechange keydir
                |> ballbounce
            , cmd
            )


paddlechange : Keydir -> Model -> Model
paddlechange keydir model =
    { model | paddle = { pos = model.paddle.pos, dir = keydir} }--need to be improved: 可不可以只再


paddlemove : Model -> Model
paddlemove model =
    let
        newx =
            paddlePosx model.paddle.pos.x model.paddle.dir
        newPoint =
            (Point newx model.paddle.pos.y)
    in
    { model | paddle = { pos = newPoint, dir = model.paddle.dir } }


ballmove : Model -> Model
ballmove model =
    { model | ball = Ball (Point (model.ball.pos.x + ((cos (degrees model.ball.dir)) * 10)) (model.ball.pos.y + ((sin (degrees model.ball.dir)) * 10))) model.ball.dir Alive ball_radius}


checkOneBrick : Brick -> Model -> Model
checkOneBrick brick model =
    let
        x = model.ball.pos.x
        y = model.ball.pos.y
        x1 = brick.pos.x
        x2 = x1 + brick_width
        y1 = brick.pos.y
        y2 = y1 + brick_height
        r = model.ball.radius
    in
        if x >= (x1 - 5) && x <= (x2 + 5) then
            if ((( y - y2) <= r && (y - y2 > 0)) || ((y1 - y) <= r && (y1 - y) > 0 )) then
                {model | ball = (changeballdir model.ball 360)}
            else
                {model| brick = (brick :: (model.brick))}
        else if y >= y1 && y <= y2 then
            if ( (x - x2 > 0 && x - x2 <= r) || (x1 - x > 0 && x1 - x <= r) )then
                {model | ball = (changeballdir model.ball 180)}
            else
                {model| brick = (brick :: (model.brick))}
        else
            {model| brick = (brick :: (model.brick))}--比较怀疑是所有的判断条件最后都走到了这里orz，于是什么都没有发生，小球被忽视了


checkoutBallBrick : Model -> Model
--把model.brick里的砖块一个个放到上面的函数里检查是不是会发生碰撞，
--如果碰撞了，球方向改变，这个砖块就碎了，不会被放进原先被清空了的旧的砖块序列，否则会照常放进去。
checkoutBallBrick model =
    let bricks = model.brick--可能这里会出问题？小概率）
    in
        List.foldr (checkOneBrick) {model | brick = []} bricks
     

    
    

checkoutBallPaddle : Model -> Model
checkoutBallPaddle model =
    let
        x = model.ball.pos.x
        y = model.ball.pos.y
        x1 = model.paddle.pos.x
        x2 = x1 + paddle_width
        d = (model.paddle.pos.y) - y
        r = model.ball.radius
    in
        if x1 <= (x + 5) && x2 >= (x - 5) && d <=r then
            {model | ball = (changeballdir model.ball 360)}
        else
            model


checkFourOutlines : Model -> Model
checkFourOutlines model =
    let
        x = model.ball.pos.x
        y = model.ball.pos.y
        r = model.ball.radius
    in
        let
            angle = 
                if x <= r || (1000 - x) <= r then
                    180
                else if y <= r || (1000 - y) <= r then 
                    360
                else
                    2 * model.ball.dir
        in
            {model | ball = (changeballdir model.ball angle )}

            




changeballdir : Ball -> Float -> Ball
changeballdir ball number =
    Ball ball.pos (number - ball.dir) ball.liveStatus ball_radius



ballbounce : Model -> Model
ballbounce model = 
    checkFourOutlines model
        |> checkoutBallPaddle
        |> checkoutBallBrick



paddlePosx : Float -> Keydir -> Float
paddlePosx oldx direction =
    case direction of
        Key_right ->
            oldx + 8.0

        Key_left ->
            oldx - 8.0

        Key_none ->
            oldx

paddle_width = 120
paddle_height = 20
drawPaddle : Paddle -> Svg msg
drawPaddle paddle =
    Svg.rect
        [ SvgAttr.width (toString paddle_width)
        , SvgAttr.height (toString paddle_height)
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString paddle.pos.x)
        , SvgAttr.y (toString paddle.pos.y)
        ]
        []


drawBrick : Brick -> Svg Msg
drawBrick brick =
    Svg.rect
        [ SvgAttr.width (toString (brick_width - 2))
        , SvgAttr.height (toString (brick_height - 2))
        , SvgAttr.fill "Green"
        , SvgAttr.x (toString brick.pos.x)
        , SvgAttr.y (toString brick.pos.y)
        ]
        []


drawBricks : List Brick -> List (Svg Msg)
drawBricks list =
    List.map drawBrick list

ball_radius = 20

drawball : Ball -> Svg Msg
drawball ball =
    Svg.circle
        [ SvgAttr.cx (toString ball.pos.x)
        , SvgAttr.cy (toString ball.pos.y)
        , SvgAttr.r (toString ball_radius)
        , SvgAttr.color "Red"
        ]
        []


drawBorder : Svg Msg
drawBorder = 
    Svg.rect
        [ SvgAttr.width "1000"
        , SvgAttr.height "1000"
        , SvgAttr.stroke "Black"
        , SvgAttr.strokeWidth "20"
        , SvgAttr.x "0"
        , SvgAttr.y "1000"
        ]
        []


fourBorders_String =
    "0 0 1000 1000"


view :
    Model
    -> Html Msg --主要是窗口数值
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width "1000"
            , SvgAttr.height "800"
            , SvgAttr.viewBox fourBorders_String
            ]
            (drawBorder :: (drawball model.ball :: (drawPaddle model.paddle :: drawBricks model.brick)))
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
