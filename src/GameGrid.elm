-- Tetris rules being used are the SRS rules (Super Rotation System):
-- https://tetris.fandom.com/wiki/SRS


module GameGrid exposing
    ( GameGridModel(..)
    , Msg
    , handleAction
    , height
    , mergeTetrominoInPlay
    , moveDown
    , moveLeft
    , moveRight
    , moveUp
    , randomTetromino
    , rotateTetromino
    , tetrominoes
    , tick
    , uninitialised
    , update
    , validTetrominoPosition
    , view
    , width
    )

import GameGridTypes exposing (Cell(..), Colour(..), Coordinate, CurrentTetromino(..), GridCell, Model, Tetromino, TetrominoInPlay)
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


uninitialised : GameGridModel
uninitialised =
    Uninitialised


type GameGridModel
    = Uninitialised
    | Initialised Model


type Msg
    = DeadCellAnimationEnd Coordinate



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
            Initialised <|
                Model
                    NoTetromino
                    (randomTetromino millis)
                    []
                    millis

        Initialised model ->
            case model.current of
                NoTetromino ->
                    model
                        |> putNextTetrominoInPlay
                        |> setNextTetromino (randomTetromino millis)
                        |> setTimestamp millis
                        |> Initialised

                InPlay tetrominoInPlay ->
                    if timeToDrop model millis then
                        tetrominoInPlay
                            |> moveDown
                            |> updateTetrominoInPlay model
                            |> setTimestamp millis
                            |> Initialised

                    else
                        Initialised model

                Landed tetrominoInPlay ->
                    Initialised model


timeToDrop : Model -> Int -> Bool
timeToDrop model millis =
    millis >= model.timestamp + dropDelay


updateTetrominoInPlay : Model -> TetrominoInPlay -> Model
updateTetrominoInPlay model tetrominoInPlay =
    { model | current = InPlay tetrominoInPlay }


setTimestamp : Int -> Model -> Model
setTimestamp millis model =
    { model | timestamp = millis }


setNextTetromino : Tetromino -> Model -> Model
setNextTetromino next model =
    { model | next = next }


putNextTetrominoInPlay : Model -> Model
putNextTetrominoInPlay model =
    let
        bottomRow =
            model.next.cells
                |> List.map .row
                |> List.maximum
                |> Maybe.withDefault 0

        bottomCellAverage =
            model.next.cells
                |> List.filter (.row >> (==) bottomRow)
                |> List.map .col
                |> List.foldl
                    (\n acc ->
                        ( Tuple.first acc + 1, Tuple.second acc + n )
                    )
                    ( 0, 0 )
                |> (\( count, sum ) -> sum // count)

        position =
            Coordinate
                ((width // 2) - bottomCellAverage - 1)
                (0 - bottomRow)
    in
    { model | current = InPlay (TetrominoInPlay model.next position) }


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
    { tetromino | cells = rotateTetrominoCells tetromino.size tetromino.cells }


rotateTetrominoCells : Int -> List Coordinate -> List Coordinate
rotateTetrominoCells size cells =
    cells |> List.map (\{ col, row } -> Coordinate (size - 1 - row) col)


moveLeft : TetrominoInPlay -> TetrominoInPlay
moveLeft =
    updateCoordinate translateLeft


moveRight : TetrominoInPlay -> TetrominoInPlay
moveRight =
    updateCoordinate translateRight


moveDown : TetrominoInPlay -> TetrominoInPlay
moveDown =
    updateCoordinate translateDown


moveUp : TetrominoInPlay -> TetrominoInPlay
moveUp =
    updateCoordinate translateUp


updateCoordinate : (Coordinate -> Coordinate) -> TetrominoInPlay -> TetrominoInPlay
updateCoordinate fn tetrominoInPlay =
    { tetrominoInPlay | position = fn tetrominoInPlay.position }


translateLeft : Coordinate -> Coordinate
translateLeft { col, row } =
    Coordinate (col - 1) row


translateRight : Coordinate -> Coordinate
translateRight { col, row } =
    Coordinate (col + 1) row


translateDown : Coordinate -> Coordinate
translateDown { col, row } =
    Coordinate col (row + 1)


translateUp : Coordinate -> Coordinate
translateUp { col, row } =
    Coordinate col (row - 1)


absoluteCells : TetrominoInPlay -> List Coordinate
absoluteCells { tetromino, position } =
    tetromino.cells
        |> List.map
            (\{ col, row } ->
                Coordinate (col + position.col) (row + position.row)
            )


mergeCurrentTetromino : CurrentTetromino -> List GridCell -> List GridCell
mergeCurrentTetromino currentTetromino gridCells =
    case currentTetromino of
        NoTetromino ->
            gridCells

        InPlay tetrominoInPlay ->
            mergeTetrominoInPlay tetrominoInPlay gridCells

        Landed tetrominoInPlay ->
            mergeTetrominoInPlay tetrominoInPlay gridCells


mergeTetrominoInPlay : TetrominoInPlay -> List GridCell -> List GridCell
mergeTetrominoInPlay tetrominoInPlay gridCells =
    gridCells
        ++ (tetrominoInPlay
                |> absoluteCells
                |> List.map (GridCell (Alive tetrominoInPlay.tetromino.colour))
           )



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


dropDelay : Int
dropDelay =
    200


getCell : Model -> Coordinate -> Cell
getCell model position =
    if isValidCoordinate position then
        model.gridCells
            |> mergeCurrentTetromino model.current
            |> List.filter (\gc -> gc.position == position)
            |> List.head
            |> Maybe.map .cell
            |> Maybe.withDefault Empty

    else
        OutOfBounds


isValidCoordinate : Coordinate -> Bool
isValidCoordinate { col, row } =
    col >= 0 && col < width && row < height


validTetrominoPosition : TetrominoInPlay -> List GridCell -> Bool
validTetrominoPosition tetrominoInPlay gridCells =
    tetrominoInPlay
        |> absoluteCells
        |> List.all
            (\pos ->
                isValidCoordinate pos
                    && not (List.any (\gc -> gc.position == pos) gridCells)
            )


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
