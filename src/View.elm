module View exposing (view)

import Axis3d exposing (y)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (..)
import Star exposing (Earth, Proton, Spacecraft, Sun, originX, originY, spcheight, spcwidth, tracradius)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (y1)
import Update exposing (dotLineDistance, getLine, xShift, yShift)


drawSpacecraft : List Spacecraft -> List (Svg msg)
drawSpacecraft spacecraft =
    List.map drawSpacecraft_Inside spacecraft |> List.concat


drawSpacecraft_Inside : Spacecraft -> List (Svg msg)
drawSpacecraft_Inside spacecraft =
    let
        an =
            ((pi / 2 - spacecraft.angle) * 180) / pi

        x =
            spacecraft.pos.x

        y =
            spacecraft.pos.y
    in
    [ Svg.line
        -- front of spacecraft
        [ SvgAttr.x1 (String.fromFloat (x - 20))
        , SvgAttr.y1 (String.fromFloat y)
        , SvgAttr.x2 (String.fromFloat x)
        , SvgAttr.y2 (String.fromFloat y)
        , SvgAttr.stroke "#de3337"
        , SvgAttr.strokeWidth (toString (0.5 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat x, " ", String.fromFloat y, ")" ])
        ]
        []
    , Svg.line
        --horizontal line
        [ SvgAttr.x1 (String.fromFloat x)
        , SvgAttr.y1 (String.fromFloat (y - 25))
        , SvgAttr.x2 (String.fromFloat x)
        , SvgAttr.y2 (String.fromFloat (y + 25))
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth (toString (0.25 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat x, " ", String.fromFloat y, ")" ])
        ]
        []
    , Svg.circle
        -- white circle
        [ SvgAttr.fill "white"
        , SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r "15"
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat x, " ", String.fromFloat y, ")" ])
        ]
        []
    , Svg.circle
        -- red circle
        [ SvgAttr.fill "#de3337"
        , SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r "5"
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat x, " ", String.fromFloat y, ")" ])
        ]
        []
    , Svg.line
        -- right wing
        [ SvgAttr.x1 (String.fromFloat (xShift x 25 + 0.5 * spcwidth))
        , SvgAttr.y1 (String.fromFloat (yShift y 25))
        , SvgAttr.x2 (String.fromFloat (xShift x 25 - 0.5 * spcwidth))
        , SvgAttr.y2 (String.fromFloat (yShift y 25))
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth (toString (0.25 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x 25), " ", String.fromFloat (yShift y 25), ")" ])
        ]
        []
    , Svg.line
        -- left wing
        [ SvgAttr.x1 (String.fromFloat (xShift x -25 - 0.5 * spcwidth))
        , SvgAttr.y1 (String.fromFloat (yShift y -25))
        , SvgAttr.x2 (String.fromFloat (xShift x -25 + 0.5 * spcwidth))
        , SvgAttr.y2 (String.fromFloat (yShift y -25))
        , SvgAttr.stroke "white"
        , SvgAttr.strokeWidth (toString (0.25 * spcheight))
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat an, " ", String.fromFloat (xShift x -25), " ", String.fromFloat (yShift y -25), ")" ])
        ]
        []
    ]


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
    let
        id =
            proton.intensity

        h =
            toString 60 ++ ", "

        s =
            toString (20 * id) ++ "%, "

        l =
            toString (50 + 10 * id) ++ "%)"

        color =
            "hsl(" ++ h ++ s ++ l
    in
    [ Svg.circle
        [ SvgAttr.cx (toString proton.pos.x)
        , SvgAttr.cy (toString proton.pos.y)
        , SvgAttr.r (toString proton.radius)
        , SvgAttr.fill color
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


drawEarth : Earth -> List (Svg msg)
drawEarth earth =
    [ Svg.circle
        [ SvgAttr.cx (toString earth.pos.x)
        , SvgAttr.cy (toString earth.pos.y)
        , SvgAttr.r "30"
        ]
        []
    , Svg.image
        [ SvgAttr.x (toString (earth.pos.x - 30))
        , SvgAttr.y (toString (earth.pos.y - 30))
        , SvgAttr.width "60"
        , SvgAttr.height "60"
        , SvgAttr.xlinkHref "Assets/Earth.png"
        ]
        []
    ]


renderInfo : Model -> Html Msg
renderInfo model =
    div
        [ style "background" "#34495f"
        , style "border" "0"
        , style "bottom" "30px"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "30px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "500px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "500px"
        ]
        [ text ("Remain chances: " ++ toString model.heart ++ "level" ++ toString model.level ++ "isi" ++ printp (getHeadProton model.proton) ++ "time" ++ toString (modBy 1000 (round model.move_timer))) ]


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
            (drawTrack ++ drawSun model.sun ++ drawSpacecraft model.spacecraft ++ drawEarth model.earth ++ List.concat (List.map drawproton model.proton))
        , renderGameButton_1 model.state
        , renderGameButton_2 model.level
        , renderInfo model
        ]


printp : Proton -> String
printp proton =
    Debug.toString proton.intensity
