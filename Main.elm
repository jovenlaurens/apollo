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
type Dir 
        = Left
        | Right
        | None

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
        ={
            posx : Float
            , dir : Dir
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
init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )
    
initModel : Model
initModel =
        Model (generatebricks( 10, 5 )) 0 (Paddle  50.0 None) (Ball 100.0 400.0 45.0 )

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

changepaddleDir : Keydir -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
changepaddleDir keydir ( model, cmd ) = 
            let
                dir1=
                        case keydir of
                            Key_right ->
                                    Right

                            Key_left ->
                                    Left

                            Key_none ->
                                    None
                
                paddle1 = Paddle model.paddle.posx dir1                       
            in 
                ( {model| paddle = paddle1}
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
            changepaddleDir keydir (model
                                    |> ballball
                                    |> ballbounce
                                    |> ballball
            ,cmd)

paddlemove : Model -> Model
paddlemove model = 
        let
            newx = paddlePosx model.paddle.posx model.paddle.dir
        in
            {model | paddle = makenewpaddle newx }

ballball : Model -> Model
ballball model = 
        let 
            newball = ballmove model.ball 
        in
            {model | ball = newball}

ballmove : Ball -> Ball
ballmove ball =
     Ball (ball.posx + ((cos (degrees ball.dir)) * 0.5)) (ball.posy + ((sin (degrees ball.dir)) * 0.5)) ball.dir

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
paddlePosx : Float  -> Dir -> Float
paddlePosx oldx direction = 
    case direction of
        Right ->
             oldx + 5.0

        Left ->
             oldx - 5.0
        
        None ->
             oldx

makenewpaddle : Float ->Paddle
makenewpaddle posx =
    Paddle posx None

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