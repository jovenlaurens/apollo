module View exposing (view)

import Debug exposing (toString)
import Geometry exposing (originX, originY, spcheight, spcwidth, tracradius)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (..)
import Star exposing (Earth, Proton, Spacecraft, Sun)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Text exposing (showText)

view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "background-color"
            (case model.submodel.state of
                Cover ->
                    "#564d7c"

                Interval ->
                    "black"

                _ ->
                    "#0e1f2f"
            )
        ]
        [ let
            st =
                model.submodel.state
          in
          if st == Cover || st == Interval then
            let
                ( w, h ) =
                    model.size

                ( wid, het ) =
                    if (2 / 3 * w) >= h then
                        ( 1.5 * h, h )

                    else
                        ( w, 2 / 3 * w )

                ( lef, to ) =
                    if (2 / 3 * w) >= h then
                        ( 0.5 * (w - wid), 0 )

                    else
                        ( 0, 0.5 * (h - het) )
            in
            div
                [ style "width" (String.fromFloat wid ++ "px")
                , style "height" (String.fromFloat het ++ "px")
                , style "position" "absolute"
                , style "left" (String.fromFloat lef ++ "px")
                , style "top" (String.fromFloat to ++ "px")
                ]
                (if st == Cover then
                    renderLogo

                 else
                    renderInterval
                )

          else
            let
                ( w, h ) =
                    model.size

                line =
                    Basics.min w h

                max_ =
                    Basics.max w h

                left =
                    if w > h then
                        0.5 * (max_ - line)

                    else
                        0

                top =
                    if w > h then
                        0

                    else
                        0.5 * (max_ - line)

            in
            div
                [ HtmlAttr.style "width" (String.fromFloat line ++ "px") --how to adjust here?
                , HtmlAttr.style "height" (String.fromFloat line ++ "px")
                , HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "left" (String.fromFloat left ++ "px")
                , HtmlAttr.style "top" (String.fromFloat top ++ "px")
                , HtmlAttr.style "background-image" "url('assets/Background.jpg')"
                ]
                [ Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    , SvgAttr.viewBox "0 0 1000 1000"
                    ]
                    (drawTrack
                        ++ drawSun model.sun
                        ++ drawSpacecraft model.spacecraft
                        ++ drawEarth model.submodel.level model.earth
                        ++ List.concat (List.map drawproton model.proton)
                    )
                , div
                    [ HtmlAttr.style "width" "100%"
                    , HtmlAttr.style "height" "100%"
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "left" "0"
                    , HtmlAttr.style "top" "0"
                    ]
                    [ renderGameButton_1 model.submodel.state
                    , renderGameButton_2 model.submodel.level
                    , renderInfo model
                    , renderLife model
                    , renderLevel model
                    , renderChatBox model
                    , renderChat model
                    , renderGameButton_3 model.submodel.state model.submodel.text_num
                    ]
                , renderAudio "assets/Bgm.mp3"
                ]
        ]


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
        , SvgAttr.r (toString (sun.radius + 30))
        , SvgAttr.fill "#e0910e"
        , SvgAttr.fillOpacity "0.3"
        ]
        []
    , Svg.circle
        --<circle cx="100" cy="50" r="40" stroke="black" stroke-width="2" fill="red"
        [ SvgAttr.cx (toString sun.pos.x)
        , SvgAttr.cy (toString sun.pos.y)
        , SvgAttr.r (toString (sun.radius + 15))
        , SvgAttr.fill "#e0910e"
        , SvgAttr.fillOpacity "0.5"
        ]
        []
    , Svg.circle
        --<circle cx="100" cy="50" r="40" stroke="black" stroke-width="2" fill="red"
        [ SvgAttr.cx (toString sun.pos.x)
        , SvgAttr.cy (toString sun.pos.y)
        , SvgAttr.r (toString (sun.radius + 5))
        , SvgAttr.fill "#e0910e"
        , SvgAttr.fillOpacity "0.7"
        ]
        []
    , Svg.circle
        --<circle cx="100" cy="50" r="40" stroke="black" stroke-width="2" fill="red"
        [ SvgAttr.cx (toString sun.pos.x)
        , SvgAttr.cy (toString sun.pos.y)
        , SvgAttr.r (toString sun.radius)
        , SvgAttr.fill "#ffb32b"
        ]
        []
    , Svg.image
        [ SvgAttr.x (toString (sun.pos.x - sun.radius))
        , SvgAttr.y (toString (sun.pos.y - sun.radius))
        , SvgAttr.width (toString (2 * sun.radius))
        , SvgAttr.height (toString (2 * sun.radius))
        , SvgAttr.xlinkHref "assets/Sun.png"
        , SvgAttr.transform (String.concat [ "rotate(", String.fromFloat sun.angle, " ", String.fromFloat 500, " ", String.fromFloat 500, ")" ])
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

                _ ->
                    ( "New game", Start )
    in
    button
        [ style "position" "absolute"
        , style "top" "2%"
        , style "left" "2%"
        , style "height" "6%"
        , style "width" "10%"
        , style "background" "#34495f"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Baskerville"
        , style "font-size" "4%"
        , style "border" "none"
        , onClick msg
        ]
        [ text txt ]


renderGameButton_2 : Int -> Html Msg
renderGameButton_2 level =
    button
        [ style "background" "#34495f"
        , style "position" "absolute"
        , style "top" "10%"
        , style "left" "2%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Baskerville"
        , style "font-size" "4%"
        , style "height" "6%"
        , style "width" "10%"
        , style "border" "none"
        , onClick (Reinit level)
        ]
        [ text "Restart" ]


renderGameButton_3 : State -> Int -> Html Msg
renderGameButton_3 state textIndex =
    button
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "5%"
        , style "height" "45%"
        , style "width" "90%"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Baskervillef"
        , style "font-size" "4%"
        , style "outline" "none"
        , style "padding" "0"
        , style "border" "none"
        , style "display"
            (if state == Playing then
                "none"

             else
                "block"
            )
        , style "background-color" "Transparent"
        , onClick (ChangeText textIndex 0)
        ]
        []


drawEarth : Int -> Earth -> List (Svg msg)
drawEarth level earth =
    [ Svg.circle
        [ SvgAttr.cx (toString earth.pos.x)
        , SvgAttr.cy (toString earth.pos.y)
        , SvgAttr.r "40"
        , SvgAttr.opacity "0.3"
        , SvgAttr.fill "#61c5ff"
        , SvgAttr.display
            (if level <= 1 then
                "none"

             else
                "block"
            )
        ]
        []
    , Svg.circle
        [ SvgAttr.cx (toString earth.pos.x)
        , SvgAttr.cy (toString earth.pos.y)
        , SvgAttr.r "34"
        , SvgAttr.opacity "0.3"
        , SvgAttr.fill "#61c5ff"
        , SvgAttr.display
            (if level <= 1 then
                "none"

             else
                "block"
            )
        ]
        []
    , Svg.circle
        [ SvgAttr.cx (toString earth.pos.x)
        , SvgAttr.cy (toString earth.pos.y)
        , SvgAttr.r "30"
        , SvgAttr.fill "#61c5ff"
        , SvgAttr.display
            (if level <= 1 then
                "none"

             else
                "block"
            )
        ]
        []
    , Svg.image
        [ SvgAttr.x (toString (earth.pos.x - 30))
        , SvgAttr.y (toString (earth.pos.y - 30))
        , SvgAttr.width "60"
        , SvgAttr.height "60"
        , SvgAttr.xlinkHref "assets/Earth.png"
        , SvgAttr.display
            (if level <= 1 then
                "none"

             else
                "block"
            )
        ]
        []
    ]


renderChatBox : Model -> Html Msg
renderChatBox model =
    div
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "5%"
        , style "height" "45%"
        , style "width" "90%"
        , style "cursor" "pointer"
        , style "outline" "none"
        , style "display"
            (if model.submodel.state == Playing then
                "none"

             else
                "block"
            )
        , style "font-family" "Baskerville"
        , style "font-size" "2em"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.rect
                [ SvgAttr.width "95%"
                , SvgAttr.height "95%"
                , SvgAttr.x "2.5%"
                , SvgAttr.y "2.5%"
                , SvgAttr.fill "#34495f"
                , SvgAttr.fillOpacity "0.7"
                , SvgAttr.stroke "white"
                , SvgAttr.strokeWidth "1%"
                , SvgAttr.rx "10%"
                , SvgAttr.ry "10%"
                , SvgAttr.strokeDasharray "10 10"
                ]
                []
            ]
        ]


renderChat : Model -> Html Msg
renderChat model =
    div
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "5%"
        , style "height" "45%"
        , style "width" "82%"
        , style "cursor" "pointer"
        , style "display"
            (if model.submodel.state == Playing then
                "none"

             else
                "block"
            )
        , style "font-family" "Baskerville"
        , style "font-size" "1.4em"
        , style "color" "White"
        , style "margin" "4% 5% 4% 5%"
        ]
        [ text (showText model.submodel.text_num) ]


renderInfo : Model -> Html Msg
renderInfo model =
    div
        [ style "background" "#0e1f2f"
        , style "border" "0"
        , style "top" "2%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Baskerville"
        , style "font-size" "1.5em"
        , style "font-weight" "300"
        , style "height" "18%"
        , style "left" "65%"
        , style "line-height" "40px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "22%"
        ]
        [ text ("Score: " ++ toString model.submodel.score)
        ]


renderLevel : Model -> Html Msg
renderLevel model =
    div
        [ style "background" "0e1f2f"
        , style "border" "0"
        , style "top" "2%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Baskerville"
        , style "font-size" "1.5em"
        , style "font-weight" "300"
        , style "height" "18%"
        , style "left" "40%"
        , style "line-height" "40px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "15%"
        ]
        [ text ("Level:" ++ " " ++ toString model.submodel.level)
        ]


renderLife : Model -> Html Msg
renderLife model =
    div
        [ style "background" "#0e1f2f"
        , style "border" "0"
        , style "top" "7%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Baskerville"
        , style "font-size" "1.5em"
        , style "font-weight" "300"
        , style "height" "2%"
        , style "left" "65%"
        , style "line-height" "40px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "23%"
        ]
        [ text ("Lifes:" ++ liveSymbol model.submodel.heart) ]


liveSymbol : Int -> String
liveSymbol lives =
    case lives of
        3 ->
            "❤❤❤"

        2 ->
            "❤❤"

        1 ->
            "❤"

        _ ->
            ""


renderAudio : String -> Html Msg
renderAudio url =
    audio
        [ src url
        , autoplay True
        , loop True
        ]
        [ text "error" ]


renderLogo : List (Html Msg)
renderLogo =
    [ video
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , src "assets/Logo.mp4"
        , autoplay True
        , loop False
        ]
        []
    , button
        [ style "background" "#34495f"
        , style "border" "0"
        , style "bottom" "40%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "12.5%"
        , style "left" "17%"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "12.7%"
        , style "background-color" "Transparent"
        , onClick PlayInterval
        ]
        []
    ]


renderInterval : List (Html Msg)
renderInterval =
    [ video
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background-color" "black"
        , src "assets/Interval.mp4"
        , autoplay True
        , loop False
        ]
        []
    ]


