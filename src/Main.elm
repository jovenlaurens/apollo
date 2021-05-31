module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Random
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



---- MODEL ----


type Dir
    = Up
    | Down
    | Left
    | Right
    | None


type Live_status
    = Alive
    | Dead


type Msg
    = Direction Dir
    | Key_None
    | Tick Float


type alias Model =
    { blocks : List Block
    , paddle : Paddle
    }


type alias Block =
    { body : ( Int, Int )
    }


type alias Paddle =
    { body : ( Int, Int )
    , dir : Dir
    , state : Live_status
    }


initModel : Model
initModel =
    Model (addblocks ( 10, 5 )) (Paddle ( 1, 0 ) None Alive)


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatePaddle msg


updatePaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePaddle msg ( model, cmd ) =
    case msg of
        Tick elapsed ->
            ( model, cmd )

        Direction dir ->
            let
                currentpaddle =
                    model.paddle
            in
            ( { model
                | paddle = changeDir currentpaddle dir
              }
            , cmd
            )

        _ ->
            ( model, cmd )


changeDir : Paddle -> Dir -> Paddle
changeDir paddle dir =
    { paddle
        | dir =
            if paddle.state == Alive then
                dir

            else
                paddle.dir
    }


addoneblock : ( Int, Int ) -> Block
addoneblock pos =
    Block ( 150 * Tuple.first pos + 2, 20 * Tuple.second pos + 2 )


addblocks : ( Int, Int ) -> List Block
addblocks size =
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
        |> List.map addoneblock



---- VIEW ----


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
            ]
            (drawPaddle model.paddle :: drawBlock model.blocks)
        ]


key : Int -> Msg
key keycode =
    case keycode of
        37 ->
            Direction Left

        39 ->
            Direction Right

        _ ->
            Key_None


makenewpaddle : ( Int, Int ) -> Paddle
makenewpaddle pos =
    Paddle ( Tuple.first pos, Tuple.second pos ) None Alive


drawPaddle : Paddle -> Svg msg
drawPaddle paddle =
    Svg.rect
        [ SvgAttr.width "80"
        , SvgAttr.height "10"
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString (Tuple.first paddle.body))
        , SvgAttr.y "700"
        ]
        []


drawBlock : List Block -> List (Svg Msg)
drawBlock list =
    List.map draw list


draw : Block -> Svg Msg
draw block =
    Svg.rect
        [ SvgAttr.width "150"
        , SvgAttr.height "20"
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString (Tuple.first block.body))
        , SvgAttr.y (toString (Tuple.second block.body))
        ]
        []



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown (Decode.map key keyCode)
        ]


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
