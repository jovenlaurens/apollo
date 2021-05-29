module Main exposing (..)

import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (fill, viewBox, x, y)


type alias Brick =
    { color : String
    , y : Int
    , show_state : Bool
    , x : Int
    }


type alias Paddle =
    { color : String
    , x : Int
    }


type alias Model =
    { bricks : List Brick
    , paddle : Paddle
    }


main : Html msg
main =
    view init


init : Model
init =
    Model generateBricks (Paddle "B59FD1" 250)


generateBricks : List Brick



--Need to be improved when making the real game


generateBricks =
    addBricks "B59FD1" 19 []
        |> addBricks "B59FD1" 39
        |> addBricks "B59FD1" 59


addBricks : String -> Int -> List Brick -> List Brick
addBricks color y oldList =
    let
        newList =
            List.range 0 9
                |> List.map (\x -> x * 50 + 1)
                |> List.map (Brick color y True)
    in
    oldList ++ newList


view : Model -> Html msg
view model =
    svg
        [ viewBox "0 0 500 500"
        , width 500
        , height 500
        ]
        (singleton (drawPaddle model.paddle) ++ drawBricks model.bricks)


drawPaddle : Paddle -> Svg msg
drawPaddle paddle =
    rect
        [ width 40
        , height 5
        , fill paddle.color
        , x (toString paddle.x)
        , y "250"
        ]
        []


drawBricks : List Brick -> List (Svg msg)
drawBricks list =
    List.map drawOneBrick list


drawOneBrick : Brick -> Svg msg
drawOneBrick brick =
    rect
        [ width 48
        , height 18
        , fill brick.color
        , x (toString brick.x)
        , y (toString brick.y)
        ]
        []
