module View exposing (view)

import Basics exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (..)
import Star exposing (Earth, Proton, Spacecraft, Sun, originX, originY, spcheight, spcwidth, tracradius)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (y1)
import Text exposing (showText)
import Update exposing (dotLineDistance, getLine)
import Html.Events exposing (on)



--elm make src/Main.elm src/Messages.elm src/Model.elm src/Star.elm src/Update.elm src/View.elm src/Point.elm src/Text.elm


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

                BeforePlay ->
                    ( "New game", Start )
                _ ->
                    ( "New game", Start )
    in
    button
        [ style "background" "#34495f"
        , style "position" "absolute"
        , style "top" "2%"
        , style "left" "2%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "4%"
        , style "height" "6%"
        , style "width" "10%"
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
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "4%"
        , style "height" "6%"
        , style "width" "10%"
        , onClick (Reinit level)
        ]
        [ text "Restart" ]


renderGameButton_3 : State -> Int -> Html Msg
renderGameButton_3 state textIndex =
    button
        [ style "background" "#34495f"
        , style "position" "absolute"
        , style "top" "89%"
        , style "left" "5%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "4%"
        , style "height" "6%"
        , style "width" "10%"
        , style "display"
            (if state == Playing then
                "none"

             else
                "block"
            )
        , onClick (ChangeText textIndex 0)
        ]
        [ text "Next" ]


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



--<rect x="50" y="20" rx="20" ry="20" width="150" height="150"
--style="fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;
--stroke-opacity:0.9


renderChatBox : Model -> Html Msg
renderChatBox model =
    div
        [ style "background" "#34495f"
        , style "position" "absolute"
        , style "top" "50%"
        , style "left" "5%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display"
            (if model.submodel.state == Playing then
                "none"

             else
                "block"
            )
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "2em"
        , style "height" "45%"
        , style "width" "90%"
        ]
        [ text (showText model.submodel.text_num) ]


renderInfo : Model -> Html Msg
renderInfo model =
    div
        [ style "background" "#34495f"
        , style "border" "0"
        , style "top" "2%"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "18%"
        , style "left" "75%"
        , style "line-height" "30px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "23%"
        ]
        [ text ("Remain chances: " ++ toString model.submodel.heart)
        , br [] []
        , text ("level: " ++ toString model.submodel.level)
        , br [] []
        , text ("isi: " ++ printp (getHeadProton model.proton))
        , br [] []
        , text ("time: " ++ toString (modBy 100 (round model.submodel.move_timer) == 0))
        ]


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


renderGameButton : String -> Html Msg
renderGameButton txt =
    button
        [ style "background" "#34495f"
        , style "border" "0"
        , style "bottom" "0"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "width" "120px"
        , style "position" "relative"
        , style "left" "440px"
        , style "top" "200px"
        , style "margin" "30px 0 0"
        , onClick EnterGame
        ]
        [ text txt ]


renderCover : Model -> Html Msg
renderCover model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background-image" "url('assets/Cover.png')"
        , HtmlAttr.style "background-size" "cover"
        ]
        [ button
            [ style "background" "#34495f"
            , style "border" "0"
            , style "bottom" "40%"
            , style "color" "#fff"
            , style "cursor" "pointer"
            , style "display" "block"
            , style "font-family" "Helvetica, Arial, sans-serif"
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


renderTitle : String -> Html Msg
renderTitle txt =
    div
        [ style "color" "white"
        , style "font-size" "100px"
        , style "line-height" "60px"
        , style "margin" "30px 0 0"
        , style "top" "100px"
        , style "position" "relative"
        ]
        [ text txt ]

renderLogo : List (Html Msg)
renderLogo =
    [video
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , src "assets/Logo.mp4"
        , autoplay True
        , loop False
        ]
        [
        ]
    , button
            [ style "bottom" "0%"
            , style "display" "block"
            , style "border" "0"
            , style "height" "100%"
            , style "left" "0%"
            , style "padding" "0"
            , style "position" "absolute"
            , style "width" "100%"
            , style "background-color" "Transparent"
            , onClick EnterCover
            ]
            []
    ]

renderInterval :List (Html Msg)
renderInterval =
    [video
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , src "assets/Interval.mp4"
        , autoplay True
        , loop False
        ]
        []
    ]



view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "background-color" "#0e1f2f"
        ]
        [ 
        let
            st = model.submodel.state
        in
        if st == BeforePlay || st == Cover || st == Interval then
            let
                ( w, h ) =
                    model.size
                (wid, het) =    if ( 2 / 3 * w ) >= h then
                                    ( 1.5 * h, h )
                                else
                                    ( w, 2 / 3 * w )
                (lef, to) = if ( 2 / 3 * w ) >= h then
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
                (if st == BeforePlay then
                    renderCover model |> List.singleton
                else if st == Cover then
                    renderLogo
                else 
                    renderInterval
                )
                    

          else
            let
                ( w, h ) =
                    model.size
                line = Basics.min w h
                max_ = Basics.max w h
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

                --窗口的大小
                --取到了最小的那个，能显示出全部
            in
            div
                [ HtmlAttr.style "width" (String.fromFloat line ++ "px")--how to adjust here?
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
                    (  drawTrack 
                    ++ drawSun model.sun 
                    ++ drawSpacecraft model.spacecraft 
                    ++ drawEarth model.submodel.level model.earth 
                    ++ List.concat (List.map drawproton model.proton))
                , 
                div
                    [ HtmlAttr.style "width" "100%"--how to adjust here?
                    , HtmlAttr.style "height" "100%"
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "left" "0"
                    , HtmlAttr.style "top" "0"
                    ]
                    [ renderGameButton_1 model.submodel.state
                    , renderGameButton_2 model.submodel.level
                    , renderInfo model
                    , renderChatBox model
                    , renderGameButton_3 model.submodel.state model.submodel.text_num
                    ]
                ,
                renderAudio "assets/Bgm.mp3"
                ]
        ]





printp : Proton -> String
printp proton =
    Debug.toString proton.intensity
