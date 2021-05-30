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
            ,posy :Int

        }
type alias Paddle
        ={
            posx : Int
            , dir : Dir
        }
--Model
type alias  Model 
        = { 
            brick : List Brick
            , move_timer : Float
            , paddle : Paddle
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
        Model (generatebricks( 10, 5 )) 0 (Paddle  50 None)
generateonebrick : ( Int, Int ) -> Brick
generateonebrick position =
        Brick (150 * Tuple.first position + 2) (20 * Tuple.second position + 2)
 
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
        |>List.map generateonebrick

--update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatepaddle msg

changeDir : Keydir -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
changeDir keydir ( model, cmd ) = 
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
            , cmd
            )

        Key keydir ->
            changeDir keydir (model,cmd)
paddlemove : Model -> Model
paddlemove model = 
        let
            newx = paddlePosx model.paddle.posx model.paddle.dir
        in
            {model | paddle = makenewpaddle newx }



paddlePosx : Int  -> Dir -> Int
paddlePosx oldx direction = 
    case direction of
        Right ->
             oldx + 5

        Left ->
             oldx - 5
        
        None ->
             oldx

makenewpaddle : Int ->Paddle
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


drawBricks : List Brick -> List (Svg Msg)
drawBricks list = 
        List.map draw list

draw : Brick -> Svg Msg
draw brick =
    Svg.rect[
                 SvgAttr.width "150"
                , SvgAttr.height "20"
                , SvgAttr.fill "Green"
                , SvgAttr.x (toString (brick.posx))
                , SvgAttr.y (toString (brick.posy))
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
            ] ((drawPaddle model.paddle) :: (drawBricks model.brick))
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