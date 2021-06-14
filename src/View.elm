module View exposing (view)

import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (..)
import Star exposing (Proton, Spacecraft, Sun, originX, originY, spcheight, spcwidth, tracradius)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (y1)
import Update exposing (dotLineDistance, getLine)


drawSpacecraft : List Spacecraft -> List (Svg msg)
drawSpacecraft spacecraft =
    List.map drawSpacecraft_Inside spacecraft |> List.concat


drawSpacecraft_Inside : Spacecraft -> List (Svg msg)
drawSpacecraft_Inside spacecraft =
    let
        an =
            ((pi / 2 - spacecraft.angle) * 180) / pi

        x_ =
            spacecraft.pos.x - 0.5 * spcwidth

        y_ =
            spacecraft.pos.y - 0.5 * spcheight

        x_1 =
            spacecraft.pos.x

        y_1 =
            spacecraft.pos.y
    in
    [ Svg.line
        [ SvgAttr.x1 (String.fromFloat ((xShift (x_ + 0.5 * spcwidth) -10 - 0.5 * spcwidth) - 20))
        , SvgAttr.y1 (String.fromFloat (yShift (y_ + 0.5 * spcheight) -10 - 0.5 * spcheight))
        , SvgAttr.x2 (String.fromFloat (xShift (x_ + 0.5 * spcwidth) -10 - 0.5 * spcwidth))
        , SvgAttr.y2 (String.fromFloat (yShift (y_ + 0.5 * spcheight) -10 - 0.5 * spcheight))
        , SvgAttr.stroke "#de3337"
        , SvgAttr.strokeWidth (toString (0.5 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x_1 -10), " ", String.fromFloat (yShift y_1 -10), ")" ])
        ]
        []
    , Svg.circle
        [ SvgAttr.fill "white"
        , SvgAttr.cx (String.fromFloat (xShift (x_ + 0.5 * spcwidth) -10 - 0.5 * spcwidth))
        , SvgAttr.cy (String.fromFloat (yShift (y_ + 0.5 * spcheight) -10 - 0.5 * spcheight))
        , SvgAttr.r "15"
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x_1 -10), " ", String.fromFloat (yShift y_1 -10), ")" ])
        ]
        []
    , Svg.line
        [ SvgAttr.x1 (String.fromFloat (xShift (x_ + 0.5 * spcwidth) -10 - 0.5 * spcwidth))
        , SvgAttr.y1 (String.fromFloat ((yShift (y_ + 0.5 * spcheight) -10 - 0.5 * spcheight) - 25))
        , SvgAttr.x2 (String.fromFloat (xShift (x_ + 0.5 * spcwidth) -10 - 0.5 * spcwidth))
        , SvgAttr.y2 (String.fromFloat ((yShift (y_ + 0.5 * spcheight) -10 - 0.5 * spcheight) + 25))
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth (toString (0.25 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x_1 -10), " ", String.fromFloat (yShift y_1 -10), ")" ])
        ]
        []
    , Svg.circle
        [ SvgAttr.fill "#de3337"
        , SvgAttr.cx (String.fromFloat (xShift (x_ + 0.5 * spcwidth) -10 - 0.5 * spcwidth))
        , SvgAttr.cy (String.fromFloat (yShift (y_ + 0.5 * spcheight) -10 - 0.5 * spcheight))
        , SvgAttr.r "5"
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x_1 -10), " ", String.fromFloat (yShift y_1 -10), ")" ])
        ]
        []
    , Svg.line
        [ SvgAttr.x1 (String.fromFloat ((xShift (x_ + 0.5 * spcwidth) 15 - 0.5 * spcwidth) + 0.5 * spcwidth))
        , SvgAttr.y1 (String.fromFloat (yShift (y_ + 0.5 * spcheight) 15 - 0.5 * spcheight))
        , SvgAttr.x2 (String.fromFloat ((xShift (x_ + 0.5 * spcwidth) 15 - 0.5 * spcwidth) - 0.5 * spcwidth))
        , SvgAttr.y2 (String.fromFloat (yShift (y_ + 0.5 * spcheight) 15 - 0.5 * spcheight))
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth (toString (0.25 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x_1 15), " ", String.fromFloat (yShift y_1 15), ")" ])
        ]
        []
    , Svg.line
        [ SvgAttr.x1 (String.fromFloat ((xShift (x_ + 0.5 * spcwidth) -35 - 0.5 * spcwidth) - 0.5 * spcwidth))
        , SvgAttr.y1 (String.fromFloat (yShift (y_ + 0.5 * spcheight) -35 - 0.5 * spcheight))
        , SvgAttr.x2 (String.fromFloat ((xShift (x_ + 0.5 * spcwidth) -35 - 0.5 * spcwidth) + 0.5 * spcwidth))
        , SvgAttr.y2 (String.fromFloat (yShift (y_ + 0.5 * spcheight) -35 - 0.5 * spcheight))
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth (toString (0.25 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x_1 -35), " ", String.fromFloat (yShift y_1 -35), ")" ])
        ]
        []
    ]


xShift : Float -> Float -> Float
xShift x r =
    originX + (((tracradius + r) / tracradius) * (x - originX))


yShift : Float -> Float -> Float
yShift y r =
    originY - (((tracradius + r) / tracradius) * (originY - y))


drawSun : Sun -> List (Svg msg)
drawSun sun =
    [ Svg.circle
        --<circle cx="100" cy="50" r="40" stroke="black" stroke-width="2" fill="red"
        [ SvgAttr.cx (toString sun.pos.x)
        , SvgAttr.cy (toString sun.pos.y)
        , SvgAttr.r (toString sun.radius)
        , SvgAttr.fill "#f2d647"
        ]
        []
    ]


drawTrack : List (Svg msg)



--stroke-dasharray="5,5" d="M5 20 l215 0"


drawTrack =
    [ Svg.circle
        [ SvgAttr.cx (toString originX)
        , SvgAttr.cy (toString originY)
        , SvgAttr.r (toString tracradius)
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeDasharray "20"
        , SvgAttr.fillOpacity "0"
        ]
        []
    ]


drawproton : Proton -> List (Svg msg)
drawproton proton =
    [ Svg.circle
        [ SvgAttr.cx (toString proton.pos.x)
        , SvgAttr.cy (toString proton.pos.y)
        , SvgAttr.r (toString proton.radius)
        , SvgAttr.fill "#ffff00"
        ]
        []
    ]


renderGameButton_1 : Model.State -> Html Msg
renderGameButton_1 state =
    let
        ( txt, msg ) =
            case state of
                Model.Stopped ->
                    ( "New game", Start )

                Model.Playing ->
                    ( "Pause", Pause )

                Model.Paused ->
                    ( "Resume", Resume )
    in
    button
        [ style "background" "#34495f"
        , style "border" "0"
        , style "bottom" "30px"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "30px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "120px"
        , onClick msg
        ]
        [ text txt ]


renderGameButton_2 : Int -> Html Msg
renderGameButton_2 level =
    button
        [ style "background" "#34495f"
        , style "border" "0"
        , style "bottom" "30px"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "180px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "120px"
        , onClick (Reinit level)
        ]
        [ text "Restart" ]


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background-color" "black"
        , HtmlAttr.style "background-image" "url('assets/Background.jpg')"
        ]
        [ Svg.svg
            [ SvgAttr.width "1000"
            , SvgAttr.height "1000"
            , SvgAttr.viewBox "0 0 1000 1000"
            ]
            (drawTrack ++ drawSun model.sun ++ drawSpacecraft model.spacecraft ++ drawproton model.proton)
        , renderGameButton_1 model.state
        , renderGameButton_2 model.level
        ]
