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

        drawApple =
            apple model.boardConf.cellSize model.apple
    in
        div []
            [ header model.gameState
            , svg [ Html.Attributes.style [ ( "border", "solid" ) ], viewBox ("0 0 " ++ width_ ++ " " ++ height_), width "800px" ]
                (drawApple
                    :: (model.snek
                            |> List.Nonempty.map drawTriangle
                            |> List.Nonempty.toList
                       )
                )
            ]


apple : Int -> Coordinates -> Svg Msg
apple size { x, y } =
    let
        offset =
            size // 2

        x_ =
            x * size

        y_ =
            y * size

        x1 =
            toString <| x_ + offset

        y1 =
            toString <| y_ + offset

        rad =
            offset |> toString
    in
        circle [ cx x1, cy y1, r rad, stroke "Aquamarine", fill "Aquamarine" ] []


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
            h1 [ Html.Attributes.style [ ( "display", "none" ) ] ] []


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
        polyline [ fill "none", stroke "pink", points (Debug.log "smth" trianglePoints) ] []
