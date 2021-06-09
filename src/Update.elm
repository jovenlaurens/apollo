module Update exposing (..)

import Messages exposing (..)
import Model exposing (..)



--update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> updatepaddle msg


changepaddleDir : Dir -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
changepaddleDir keydir ( model, cmd ) =
    let
        dir1 =
            case keydir of
                Right ->
                    Right

                Left ->
                    Left

                None ->
                    None

        paddle1 =
            Paddle model.paddle.posx dir1
    in
    ( { model | paddle = paddle1 }
    , cmd
    )


updatepaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatepaddle msg ( model, cmd ) =
    case msg of
        Tick elapsed ->
            ( { model | move_timer = model.move_timer + elapsed }
                |> paddlemove
                |> ballbounce
                |> ballball
            , cmd
            )

        Key keydir ->
            changepaddleDir keydir
                ( model
                    |> ballball
                    |> ballbounce
                    |> ballball
                , cmd
                )


paddlemove : Model -> Model
paddlemove model =
    let
        newx =
            paddlePosx model.paddle.posx model.paddle.dir
    in
    { model | paddle = makenewpaddle newx }


ballball : Model -> Model
ballball model =
    let
        newball =
            ballmove model.ball
    in
    { model | ball = newball }


ballmove : Ball -> Ball
ballmove ball =
    Ball (ball.posx + (cos (degrees ball.dir) * 0.5)) (ball.posy + (sin (degrees ball.dir) * 0.5)) ball.dir 10


changeballdir : Ball -> Float -> Ball
changeballdir ball number =
    Ball ball.posx ball.posy (number - ball.dir) 10


ballbounce : Model -> Model
ballbounce model =
    if model.ball.posy >= screen_height - model.ball.radius then
        { model | ball = changeballdir model.ball 360.0 }

    else if model.ball.posx >= screen_width - model.ball.radius then
        { model | ball = changeballdir model.ball 540.0 }

    else if model.ball.posx <= 0 + model.ball.radius then
        { model | ball = changeballdir model.ball 180.0 }

    else if model.ball.posy <= 0 + model.ball.radius then
        { model | ball = changeballdir model.ball 360.0 }

    else
        model


paddlePosx : Float -> Dir -> Float
paddlePosx oldx direction =
    case direction of
        Right ->
            oldx + 8.0

        Left ->
            oldx - 8.0

        None ->
            oldx


makenewpaddle : Float -> Paddle
makenewpaddle posx =
    Paddle posx None
