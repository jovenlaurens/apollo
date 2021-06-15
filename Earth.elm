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
    in
         if (distance posp pose) <= (rp + re)  then
                loseheart model
         else
            model     
loseheart : Model -> Model
loseheart model =
    let
        heart_ = model.heart - 1
        
    in
        {model | heart = heart_ }   