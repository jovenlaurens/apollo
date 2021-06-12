module View exposing (view)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Attributes exposing (y1)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Model exposing(Model)
import Messages exposing (Msg(..))
import Star exposing (Spacecraft, spcwidth, spcheight, originX, originY)
import Debug exposing (toString)
import Star exposing (Sun, Proton)
import Star exposing (tracradius)

drawSpacecraft : Spacecraft -> List (Svg msg)
drawSpacecraft spacecraft =
    let
        an = (String.fromFloat ( (( pi/2 - spacecraft.angle) * 180) / pi )) 
        x_ = (String.fromFloat ( spacecraft.pos.x - 0.5 * spcwidth ))
        y_ = (String.fromFloat ( spacecraft.pos.y - 0.5 * spcheight ))
        x_1 = (String.fromFloat (spacecraft.pos.x))
        y_1 = (String.fromFloat (spacecraft.pos.y))

    in
    
    [Svg.rect
        [ SvgAttr.width (toString spcwidth)
        , SvgAttr.height (toString spcheight)
        , SvgAttr.fill "Blue"
        , SvgAttr.x x_
        , SvgAttr.y y_
        , SvgAttr.transform (String.concat [ "rotate(" , an," ", x_1," ", y_1,  ")" ])
        ]--(( pi/2 - spacecraft.angle) * 180) / pi
        []]

drawSun :Sun -> List(Svg msg)
drawSun sun =
    [Svg.circle--<circle cx="100" cy="50" r="40" stroke="black" stroke-width="2" fill="red"
        [ SvgAttr.cx (toString sun.pos.x)
        , SvgAttr.cy (toString sun.pos.y)
        , SvgAttr.r (toString sun.radius)
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.fill "red"
        ]
        []]

drawTrack : List (Svg msg)--stroke-dasharray="5,5" d="M5 20 l215 0"
drawTrack =
    [
        Svg.circle
        [SvgAttr.cx (toString originX)
        , SvgAttr.cy (toString originY)
        , SvgAttr.r (toString tracradius)
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeDasharray "5,5"
        ]
        []
    ]

drawproton : Proton -> List (Svg Msg)
drawproton proton = 
    [Svg.circle
        [ SvgAttr.cx (toString proton.pos.x)
        , SvgAttr.cy (toString proton.pos.y)
        , SvgAttr.r (toString proton.radius)
        , SvgAttr.fill "blue"
        ][]]

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
            [ SvgAttr.width "500"
            , SvgAttr.height "500"
            , SvgAttr.viewBox "0 0 1000 1000" 
            ]
            ((drawTrack)++(drawSun model.sun)++(drawSpacecraft model.spacecraft)++(drawproton model.proton))
        ]
