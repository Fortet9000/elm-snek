module Types exposing (..)

import List.Nonempty exposing (Nonempty)
import Time exposing (..)
import Keyboard exposing (..)


type alias Model =
    { snek : Snek
    , bearing : Bearing
    , apple : Coordinates
    , boardConf :
        { cellSize : Int
        , boardSize : { width : Int, height : Int }
        }
    , quedKeyPress : Maybe Bearing
    , gameState : GameState
    }


type alias Acc =
    { result : List Coordinates
    , neighbour : Coordinates
    }


type GameState
    = Running
    | Stopped Reason


type Reason
    = Won
    | Lost
    | NotStarted


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
