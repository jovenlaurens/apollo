module View exposing (..)

import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


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
