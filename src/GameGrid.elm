-- Tetris rules being used are the SRS rules (Super Rotation System):
-- https://tetris.fandom.com/wiki/SRS


module GameGrid exposing
    ( Cell(..)
    , CurrentTetromino(..)
    , GameGridModel(..)
    , Msg
    , Tetromino
    , handleAction
    , init
    , mergeTetrominoInPlay
    , randomTetromino
    , rotateTetromino
    , rotateTetrominoCells
    , tetrominoes
    , tick
    , update
    , view
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Decode
import PlayerAction



{-
   @@   @@  @@@@  @@@@@  @@@@@@ @@
   @@@ @@@ @@  @@ @@  @@ @@     @@
   @@ @ @@ @@  @@ @@  @@ @@@@   @@
   @@   @@ @@  @@ @@  @@ @@     @@
   @@   @@  @@@@  @@@@@  @@@@@@ @@@@@@
-}


init : GameGridModel
init =
    Uninitialised


type GameGridModel
    = Uninitialised
    | Initialised Model


type alias Model =
    { current : CurrentTetromino
    , next : Tetromino
    , gridCells : List GridCell
    }


type CurrentTetromino
    = NoTetromino
    | InPlay TetrominoInPlay
    | Landed TetrominoInPlay


type alias TetrominoInPlay =
    { tetromino : Tetromino
    , position : Coordinate
    }


type alias GridCell =
    { cell : Cell
    , position : Coordinate
    }


type Cell
    = Empty
    | Alive Colour
    | Dead Colour
    | OutOfBounds


type alias Coordinate =
    { col : Int
    , row : Int
    }


type Msg
    = DeadCellAnimationEnd Coordinate


type Colour
    = Cyan
    | Yellow
    | Magenta
    | Orange
    | Green
    | Blue
    | Red


type alias Tetromino =
    { size : Int
    , colour : Colour
    , cells : List Coordinate
    }



{-
   @@  @@ @@@@@@ @@@@@@ @@   @@
   @@  @@   @@   @@     @@   @@
   @@  @@   @@   @@@@   @@ @ @@
    @@@@    @@   @@     @@@@@@@
     @@   @@@@@@ @@@@@@  @@ @@
-}


view : GameGridModel -> Html Msg
view model =
    case model of
        Initialised gameGridModel ->
            viewGameGrid gameGridModel

        _ ->
            text "Initialising..."


viewGameGrid : Model -> Html Msg
viewGameGrid model =
    let
        drawCell coordinate cell =
            div
                [ class "GameArea_cell" ]
                [ div
                    (List.append
                        [ class <| cellClass cell ]
                        (deadCellAnimationHook cell coordinate)
                    )
                    []
                ]

        captureAnimationEnd : Coordinate -> List (Html.Attribute Msg)
        captureAnimationEnd coordinate =
            [ "webkitAnimationEnd", "oanimationend", "msAnimationEnd", "animationend" ]
                |> List.map
                    (\event ->
                        on event (Decode.succeed (DeadCellAnimationEnd coordinate))
                    )

        deadCellAnimationHook : Cell -> Coordinate -> List (Html.Attribute Msg)
        deadCellAnimationHook cell coordinate =
            case cell of
                Dead _ ->
                    captureAnimationEnd coordinate

                _ ->
                    []

        cellClass cell =
            case cell of
                Empty ->
                    "cell_empty"

                Alive block ->
                    "cell_" ++ blockClass block

                Dead block ->
                    "cell_dead cell_" ++ blockClass block

                OutOfBounds ->
                    ""

        blockClass block =
            case block of
                Red ->
                    "red"

                Green ->
                    "green"

                Blue ->
                    "blue"

                Cyan ->
                    "cyan"

                Yellow ->
                    "yellow"

                Magenta ->
                    "magenta"

                Orange ->
                    "orange"

        getCell_ x y =
            getCell model (Coordinate x y)
    in
    div
        [ class "GameArea" ]
        (List.range 0 (height - 1)
            |> List.map
                (\y ->
                    div [ class "GameArea_row" ]
                        (List.range 0 (width - 1)
                            |> List.map (\x -> drawCell (Coordinate x y) (getCell_ x y))
                        )
                )
        )



{-
   @@  @@ @@@@@  @@@@@   @@@@  @@@@@@ @@@@@@
   @@  @@ @@  @@ @@  @@ @@  @@   @@   @@
   @@  @@ @@@@@  @@  @@ @@@@@@   @@   @@@@
   @@  @@ @@     @@  @@ @@  @@   @@   @@
    @@@@  @@     @@@@@  @@  @@   @@   @@@@@@
-}


update : Msg -> GameGridModel -> GameGridModel
update msg model =
    model


tick : Int -> GameGridModel -> GameGridModel
tick millis gameGridModel =
    case gameGridModel of
        Uninitialised ->
            Initialised
                { current = NoTetromino
                , next = randomTetromino millis
                , gridCells = []
                }

        Initialised _ ->
            gameGridModel


setNextTetromino : Tetromino -> Model -> Model
setNextTetromino next model =
    { model | next = next }


setNextToCurrent : Model -> Model
setNextToCurrent model =
    { model | current = InPlay (TetrominoInPlay model.next (Coordinate 0 0)) }


handleAction : PlayerAction.Action -> GameGridModel -> GameGridModel
handleAction action model =
    case action of
        PlayerAction.RotateLeft ->
            model

        PlayerAction.RotateRight ->
            model

        PlayerAction.Left ->
            model

        PlayerAction.Right ->
            model

        PlayerAction.Down ->
            model

        PlayerAction.Drop ->
            model

        PlayerAction.None ->
            model


rotateTetromino : Tetromino -> Tetromino
rotateTetromino tetromino =
    { tetromino | cells = rotateTetrominoCells tetromino.cells }


rotateTetrominoCells : List Coordinate -> List Coordinate
rotateTetrominoCells cells =
    cells |> List.map (\{ col, row } -> Coordinate (3 - row) col)


mergeTetrominoInPlay : TetrominoInPlay -> List GridCell -> List GridCell
mergeTetrominoInPlay tetrominoInPlay gridCells =
    let
        translation : Coordinate
        translation =
            tetrominoInPlay.position

        translatedCoordinates : List Coordinate
        translatedCoordinates =
            tetrominoInPlay.tetromino.cells
                |> List.map
                    (\{ col, row } ->
                        Coordinate (col + translation.col) (row + translation.row)
                    )

        toGridCell : Coordinate -> GridCell
        toGridCell =
            GridCell (Alive tetrominoInPlay.tetromino.colour)

        newGridCells : List GridCell
        newGridCells =
            translatedCoordinates |> List.map toGridCell
    in
    gridCells ++ newGridCells



{-
    @@@@  @@@@@@ @@@@@@ @@@@@@ @@@@@@ @@@@@   @@@@
   @@     @@       @@     @@   @@     @@  @@ @@
   @@ @@@ @@@@     @@     @@   @@@@   @@@@@   @@@@
   @@  @@ @@       @@     @@   @@     @@  @@     @@
    @@@@  @@@@@@   @@     @@   @@@@@@ @@  @@  @@@@
-}


width : number
width =
    10


height : Int
height =
    20


getCell : Model -> Coordinate -> Cell
getCell model position =
    if isValidCoordinate position then
        model.gridCells
            |> List.filter (\gc -> gc.position == position)
            |> List.head
            |> Maybe.map .cell
            |> Maybe.withDefault Empty

    else
        OutOfBounds


isValidCoordinate : Coordinate -> Bool
isValidCoordinate { col, row } =
    col >= 0 && col < width && row >= 0 && row < height


cellIsEmpty : Cell -> Bool
cellIsEmpty cell =
    cell == Empty


cellIsAlive : Cell -> Bool
cellIsAlive cell =
    case cell of
        Alive _ ->
            True

        _ ->
            False


cellIsDead : Cell -> Bool
cellIsDead cell =
    case cell of
        Dead _ ->
            True

        _ ->
            False


i : Tetromino
i =
    { size = 4
    , colour = Cyan
    , cells = [ Coordinate 0 1, Coordinate 1 1, Coordinate 2 1, Coordinate 3 1 ]
    }


o : Tetromino
o =
    { size = 4
    , colour = Yellow
    , cells = [ Coordinate 1 1, Coordinate 2 1, Coordinate 1 2, Coordinate 2 2 ]
    }


t : Tetromino
t =
    { size = 3
    , colour = Magenta
    , cells = [ Coordinate 0 0, Coordinate 1 0, Coordinate 2 0, Coordinate 1 1 ]
    }


l : Tetromino
l =
    { size = 3
    , colour = Orange
    , cells = [ Coordinate 1 0, Coordinate 1 1, Coordinate 1 2, Coordinate 2 2 ]
    }


j : Tetromino
j =
    { size = 3
    , colour = Blue
    , cells = [ Coordinate 1 0, Coordinate 1 1, Coordinate 1 2, Coordinate 0 2 ]
    }


s : Tetromino
s =
    { size = 3
    , colour = Green
    , cells = [ Coordinate 0 1, Coordinate 1 1, Coordinate 1 0, Coordinate 2 0 ]
    }


z : Tetromino
z =
    { size = 3
    , colour = Red
    , cells = [ Coordinate 0 0, Coordinate 1 0, Coordinate 1 1, Coordinate 2 1 ]
    }


tetrominoes :
    { i : Tetromino
    , o : Tetromino
    , t : Tetromino
    , l : Tetromino
    , j : Tetromino
    , s : Tetromino
    , z : Tetromino
    }
tetrominoes =
    { i = i, o = o, t = t, l = l, j = j, s = s, z = z }


randomTetromino : Int -> Tetromino
randomTetromino seed =
    case seed |> modBy 7 of
        0 ->
            i

        1 ->
            o

        2 ->
            t

        3 ->
            l

        4 ->
            j

        5 ->
            s

        _ ->
            z
