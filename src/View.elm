module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import List.Nonempty exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (Model, Msg(..), GameState(..), Reason(..), Coordinates)


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
        div []
            [ header model.gameState
            , svg [ viewBox ("0 0 " ++ width_ ++ " " ++ height_), width "800px" ]
                (model.snek
                    |> List.Nonempty.map drawTriangle
                    |> List.Nonempty.toList
                )
            ]


header : GameState -> Html Msg
header gameStatus =
    case gameStatus of
        Stopped reason ->
            case reason of
                NotStarted ->
                    h1 [ Html.Attributes.style [ ( "position", "absolute" ) ] ] [ Html.text "Press any key to start" ]

                Won ->
                    h1 [ Html.Attributes.style [ ( "position", "absolute" ) ] ] [ Html.text "You won" ]

                Lost ->
                    h1 [ Html.Attributes.style [ ( "position", "absolute" ) ] ] [ Html.text "You died" ]

        _ ->
            h1 [] []


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
