module View exposing (view)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Attributes exposing (y1)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Model exposing(Model)
import Messages exposing (Msg(..))
import Star exposing (Spacecraft, spcwidth, spcheight)
import Debug exposing (toString)
import Star exposing (Point)
import Star exposing (Proton)
import Star exposing (Sun)
drawSpacecraft : Spacecraft -> Svg Msg
drawSpacecraft spacecraft =
    Svg.rect
        [ SvgAttr.width (toString spcwidth)
        , SvgAttr.height (toString spcheight)
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString spacecraft.pos.x)
        , SvgAttr.y (toString spacecraft.pos.y)
        , SvgAttr.transform (String.concat [ "rotate(" , (String.fromFloat ( (( pi/2 - spacecraft.angle) * 180) / pi )) ," ", (String.fromFloat ( spacecraft.pos.x ))," ", (String.fromFloat ( spacecraft.pos.y )),  ")" ])
        ]
        []

drawproton : Proton -> Svg Msg
drawproton proton = 
    Svg.circle
        [ SvgAttr.cx (toString proton.pos.x)
        , SvgAttr.cy (toString proton.pos.y)
        , SvgAttr.r (toString proton.radius)
        , SvgAttr.color "blue"
        ][]
        
drawsun : Sun -> Svg Msg
drawsun sun =
    Svg.circle
        [ SvgAttr.cx (toString sun.pos.x)
        , SvgAttr.cy (toString sun.pos.y)
        , SvgAttr.r (toString sun.radius)
        , SvgAttr.color "blue"
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
            [ SvgAttr.width "1000"
            , SvgAttr.height "1000"
            ]
           (drawsun model.sun :: ( drawproton model.proton :: (List.singleton (drawSpacecraft model.spacecraft))))
        ]
