module Update exposing (..)
import Model exposing (Model)
import Messages exposing (Msg(..), Keydir(..))
import Svg.Attributes exposing (mode)
import Star exposing (tracradius, spcheight, spcwidth, Point)
import Star exposing (Earth)


--in the update part, spacescraft is exposed as spc 
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    ( model, Cmd.none )  
        |> updatespc msg

updatespc : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatespc msg ( model, cmd ) =
    case msg of 
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
                |> protonbounce
                |> spcmove
                |> protonmove
            , cmd
            )

        Key keydir ->
            ( model
                |> spcdirchange keydir
            , cmd
            )
        _ -> 
            ( model ,cmd)
earthmove : Model -> Model
earthmove model = 
    let 
        newangle =
            earthangle model.earth.angle model.earth.velocity
        newPoint =
            earthpostrans newangle model.earth.radius
        earth_ = model.earth    
    in
        { model | earth = { earth_ | angle =newangle, pos = newPoint } }

earthpostrans : Float -> Float -> Point
earthpostrans angle  radius= 
        let
            posx = 
                    ( 500.0 + radius * cos (angle) )
            posy = 
                    ( 500.0 - radius * sin (angle) )
        in
            Point posx posy
earthangle : Float -> Float -> Float
earthangle angle velocity = 
            angle + velocity

checkoutearth : Model -> Model  
checkoutearth model =
    let
        posp = 
            model.proton.pos
        pose = 
            model.earth.pos
        rp = 
            model.proton.radius
        re = 
            model.earth.radius
        ap2s =
            anglebtwpoint posp pose
        ap = 
            model.proton.dir
        newdir = 
            (2 * ap2s) - pi - ap
    in
         if (distance posp pose) <= (rp + re)  then
            model.heart
         else

            model     
loseheart : Model -> Model
loseheart model =
    let
        heart_ = model.heart - 1
        
        
    in
        {model | heart = heart_ }   
spcdirchange : Keydir -> Model -> Model
spcdirchange keydir model =
    { model | spacecraft = { pos = model.spacecraft.pos, angle = model.spacecraft.angle
                                , dir = keydir, velocity = model.spacecraft.velocity} }

spcmove : Model -> Model
spcmove model =
    let 
        newangle =
            spcangle model.spacecraft.angle model.spacecraft.velocity model.spacecraft.dir
        newPoint =
            spcpostrans newangle
    in
        { model | spacecraft = { pos = newPoint, angle = newangle, dir = model.spacecraft.dir, velocity = model.spacecraft.velocity } }

spcangle : Float -> Float -> Keydir -> Float
spcangle angle velocity direction = 
        case direction of
        Key_right ->
            angle - velocity

        Key_left ->
            angle + velocity

        Key_none ->
            angle

spcpostrans : Float -> Point
spcpostrans angle = 
        let
            posx = 
                    ( 500.0 + tracradius * cos (angle) ) - ( spcwidth / 2 )
            posy = 
                    ( 500.0 - tracradius * sin (angle) ) - ( spcheight / 2 )

        in
            Point posx posy

protonmove : Model -> Model
protonmove model = 
        let
            newproton = move model.proton model.proton.dir model.proton.velocity
        in
            {model | proton = newproton}

move : { a | pos : Point } -> Float -> Float -> { a | pos : Point }
move element direction velocity=
    let
        point =
            element.pos

        movePoint : Point -> Point
        movePoint oldpoint =
            { oldpoint | x = oldpoint.x + velocity * cos (direction), y = oldpoint.y - velocity * sin (direction) }
    in
    { element | pos = movePoint point }


protonbounce : Model -> Model
protonbounce model =
        model
            |> checkoutsun
            |> checkoutspc

distance : Point -> Point -> Float
distance pa pb = 
        let
            ax = pa.x
            ay = pa.y
            bx = pb.x 
            by = pb.y
        in
        
        sqrt((ax - bx) ^ 2 + (ay - by) ^ 2)

checkoutsun : Model -> Model  
checkoutsun model =
    let
        posp = 
            model.proton.pos
        poss = 
            model.sun.pos
        rp = 
            model.proton.radius
        rs = 
            model.sun.radius
        ap2s =
            anglebtwpoint posp poss
        ap = 
            model.proton.dir
        newdir = 
            (2 * ap2s) - pi - ap
    in
         if (distance posp poss) <= (rp + rs)  then
            let
                proton_ = model.proton
            in
                { model | proton = { proton_ | dir = newdir} }
            
         else
            model

anglebtwpoint : Point -> Point -> Float
anglebtwpoint pa pb= 
        atan2 (pb.y - pa.y) (pa.x - pb.x) 

checkoutspc : Model -> Model
checkoutspc model =
        model 