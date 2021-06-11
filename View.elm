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

drawSpacecraft : Spacecraft -> Svg msg
drawSpacecraft spacecraft =
    Svg.rect
        [ SvgAttr.width (toString spcwidth)
        , SvgAttr.height (toString spcheight)
        , SvgAttr.fill "Blue"
        , SvgAttr.x (toString spacecraft.pos.x)
        , SvgAttr.y (toString spacecraft.pos.y)
        , SvgAttr.transform (String.concat [ "rotate(" , (String.fromFloat ( (( pi/2 - spacecraft.angle) * 180) / pi )) ," ", (String.fromFloat ( spacecraft.pos.x ))," ", (String.fromFloat ( spacecraft.pos.y )),  ")" ])
        ]--(( pi/2 - spacecraft.angle) * 180) / pi
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
            [ SvgAttr.width "1000"
            , SvgAttr.height "1000"
            ]
            (List.singleton (drawSpacecraft model.spacecraft))
        ]
