module Update exposing (..)
import Model exposing (Model)
import Messages exposing (Msg(..), Keydir(..))
import Svg.Attributes exposing (mode)
import Star exposing (tracradius, spcheight, spcwidth, Point)


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
                |> spcmove
            , cmd
            )

        Key keydir ->
            ( model
                |> spcdirchange keydir
            , cmd
            )
        _ -> 
            ( model ,cmd)

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
        


