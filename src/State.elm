module State exposing (init, update, subscriptions)

import List.Nonempty exposing (..)
import Types exposing (..)
import Keyboard exposing (..)
import Time exposing (every, second)


init : ( Model, Cmd Msg )
init =
    ( initState, Cmd.none )


initState : Model
initState =
    { snek = initSnek
    , bearing = East
    , apple = { x = 4, y = 4 }
    , boardConf =
        { cellSize = 10
        , boardSize = { width = 100, height = 100 }
        }
    , quedKeyPress = Nothing
    , gameState = Stopped NotStarted
    }


initSnek : Nonempty { x : Int, y : Int }
initSnek =
    Nonempty { x = 1, y = 0 } [ { x = 0, y = 0 }, { x = 0, y = 1 }, { x = 0, y = 2 }, { x = 0, y = 3 }, { x = 0, y = 4 } ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        Tick t ->
            let
                bearing =
                    case model.quedKeyPress of
                        Just key ->
                            newBearingIfValidTurn key model.bearing

                        Nothing ->
                            model.bearing
            in
                { model | bearing = bearing, quedKeyPress = Nothing } |> moveSnek |> checkCollision

        KeyboardTjofras keyCode ->
            keyPress model keyCode


checkCollision : Model -> Model
checkCollision model =
    case outOfBounds model.snek model.boardConf of
        True ->
            { model | gameState = Stopped Lost }

        False ->
            model


outOfBounds : Snek -> { a | cellSize : Int, boardSize : { b | width : Int, height : Int } } -> Bool
outOfBounds snek boardConf =
    let
        h =
            head snek

        bottomBorder =
            boardConf.boardSize.height // boardConf.cellSize

        rightBorder =
            boardConf.boardSize.width // boardConf.cellSize
    in
        if h.x >= rightBorder || h.x < 0 || h.y < 0 || h.y >= bottomBorder then
            True
        else
            False


keyPress : Model -> KeyCode -> Model
keyPress model keyCode =
    case model.gameState of
        Running ->
            let
                wannaturnto =
                    case keyCode of
                        37 ->
                            Just West

                        38 ->
                            Just North

                        39 ->
                            Just East

                        40 ->
                            Just South

                        _ ->
                            Nothing
            in
                { model | quedKeyPress = wannaturnto }

        Stopped _ ->
            { initState | gameState = Running }


newBearingIfValidTurn : Bearing -> Bearing -> Bearing
newBearingIfValidTurn requestedBearing currentBearing =
    if currentBearing == East || currentBearing == West then
        if requestedBearing == North || requestedBearing == South then
            requestedBearing
        else
            currentBearing
    else if requestedBearing == East || requestedBearing == West then
        requestedBearing
    else
        currentBearing


moveSnek : Model -> Model
moveSnek model =
    let
        moveSnek : Snek -> Bearing -> Snek
        moveSnek snek bearing =
            let
                newHead =
                    head snek |> moveCellFunction bearing
            in
                Nonempty newHead (moveBody (head snek) (tail snek))
    in
        { model | snek = moveSnek model.snek model.bearing }


moveBody : Coordinates -> List Coordinates -> List Coordinates
moveBody headCoordinates tail =
    tail
        |> List.foldl moveSegment { result = [], neighbour = headCoordinates }
        |> .result


moveCellFunction : Bearing -> Coordinates -> Coordinates
moveCellFunction bearing coord =
    case bearing of
        North ->
            { coord | y = coord.y - 1 }

        East ->
            { coord | x = coord.x + 1 }

        South ->
            { coord | y = coord.y + 1 }

        West ->
            { coord | x = coord.x - 1 }


moveSegment : Coordinates -> Acc -> Acc
moveSegment me acc =
    { acc | result = acc.result ++ [ acc.neighbour ], neighbour = me }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Stopped reason ->
            Sub.batch [ (Keyboard.downs KeyboardTjofras) ]

        _ ->
            Sub.batch [ (Time.every (second / 5) Tick), (Keyboard.downs KeyboardTjofras) ]
