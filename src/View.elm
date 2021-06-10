module View exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Messages exposing (Msg(..))
import Model exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)


view :
    Model
    -> Html Msg
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
            , SvgAttr.viewBox "0 0 1000 1000"
            ]
            []
        ]
