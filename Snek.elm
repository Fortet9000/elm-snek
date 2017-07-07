module Snek exposing (..)

import Html exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import List.Nonempty exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Acc =
    { result : List Coordinates
    , neighbour : Coordinates
    }


moveSegment : Coordinates -> Acc -> Acc
moveSegment me acc =
    { acc | result = acc.result ++ [ acc.neighbour ], neighbour = me }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        Tick t ->
            moveSnek model

        KeyboardTjofras keyCode ->
            let
                wannaturnto =
                    case keyCode of
                        37 ->
                            West

                        38 ->
                            North

                        39 ->
                            East

                        40 ->
                            South

                        _ ->
                            model.bearing
            in
                { model | bearing = (newBearingIfValidTurn wannaturnto model.bearing) }


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


init : ( Model, Cmd msg )
init =
    ( { snek = Nonempty { x = 1, y = 0 } [ { x = 0, y = 0 }, { x = 0, y = 1 }, { x = 0, y = 2 }, { x = 0, y = 3 }, { x = 0, y = 4 } ]
      , bearing = East
      , apple = { x = 10, y = 10 }
      , boardConf =
            { cellSize = 10
            , boardSize = { width = 100, height = 100 }
            }
      }
    , Cmd.none
    )


type alias Model =
    { snek : Snek
    , bearing : Bearing
    , apple : Coordinates
    , boardConf :
        { cellSize : Int
        , boardSize : { width : Int, height : Int }
        }
    }


type Bearing
    = North
    | West
    | East
    | South


type alias Coordinates =
    { x : Int
    , y : Int
    }


type alias Snek =
    Nonempty Coordinates


type Cell
    = Snek
    | Apple
    | NoSnek


type Msg
    = Tick Time
    | KeyboardTjofras KeyCode


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ (Time.every (second / 5) Tick), (Keyboard.downs KeyboardTjofras) ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        width_ =
            toString model.boardConf.boardSize.width

        height_ =
            toString model.boardConf.boardSize.height

        drawTriangle =
            triangle model.boardConf.cellSize
    in
        svg [ viewBox ("0 0 " ++ width_ ++ " " ++ height_), width "800px" ]
            (model.snek
                |> List.Nonempty.map drawTriangle
                |> List.Nonempty.toList
            )


triangle : Int -> Coordinates -> Svg Msg
triangle size { x, y } =
    let
        x_ =
            x * size

        y_ =
            y * size

        x1 =
            toString x_

        y1 =
            toString y_

        x1y1 =
            x1 ++ "," ++ y1

        x2 =
            toString x_

        y2 =
            toString (y_ + size)

        x2y2 =
            x2 ++ "," ++ y2

        x3 =
            toString (x_ + size)

        y3 =
            toString (y_ + size // 2)

        x3y3 =
            x3 ++ "," ++ y3

        trianglePoints =
            x1y1 ++ " " ++ x3y3 ++ " " ++ x2y2
    in
        polyline [ fill "none", stroke "pink", points trianglePoints ] []
