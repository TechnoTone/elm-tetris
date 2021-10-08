module GameGridTests exposing (..)

import Expect exposing (equal)
import Fuzz exposing (int)
import GameGrid exposing (GameGridModel(..))
import GameGridTypes exposing (Cell(..), Coordinate, Tetromino)
import Test exposing (Test, describe, fuzz, test)


tetrominoRandomisation : Test
tetrominoRandomisation =
    describe "Tetromino Randomisation"
        [ test "I" (GameGrid.randomTetromino 0 |> equal GameGrid.tetrominoes.i |> always)
        , test "O" (GameGrid.randomTetromino 1 |> equal GameGrid.tetrominoes.o |> always)
        , test "T" (GameGrid.randomTetromino 2 |> equal GameGrid.tetrominoes.t |> always)
        , test "L" (GameGrid.randomTetromino 3 |> equal GameGrid.tetrominoes.l |> always)
        , test "J" (GameGrid.randomTetromino 4 |> equal GameGrid.tetrominoes.j |> always)
        , test "S" (GameGrid.randomTetromino 5 |> equal GameGrid.tetrominoes.s |> always)
        , test "Z" (GameGrid.randomTetromino 6 |> equal GameGrid.tetrominoes.z |> always)
        , fuzz int
            "randomly selects a tetromino from any input"
            (GameGrid.randomTetromino >> always Expect.pass)
        , fuzz int
            "all tetrominoes have size of 3 or 4"
            (GameGrid.randomTetromino >> .size >> Expect.all [ Expect.atLeast 3, Expect.atMost 4 ])
        , fuzz int
            "all tetrominoes have 4 cells"
            (GameGrid.randomTetromino >> .cells >> List.length >> equal 4)
        ]


tetrominoRotation : Test
tetrominoRotation =
    describe "Tetromino Rotation"
        [ describe "Size 4 Tetromino"
            [ test "not rotated"
                (GameGrid.tetrominoes.i.cells
                    |> equal [ xy 0 1, xy 1 1, xy 2 1, xy 3 1 ]
                    |> always
                )
            , test "rotated once"
                (GameGrid.tetrominoes.i
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 2 0, xy 2 1, xy 2 2, xy 2 3 ]
                    |> always
                )
            , test "rotated twice"
                (GameGrid.tetrominoes.i
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 3 2, xy 2 2, xy 1 2, xy 0 2 ]
                    |> always
                )
            , test "rotated three times"
                (GameGrid.tetrominoes.i
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 1 3, xy 1 2, xy 1 1, xy 1 0 ]
                    |> always
                )
            , test "rotated four times"
                (GameGrid.tetrominoes.i
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 0 1, xy 1 1, xy 2 1, xy 3 1 ]
                    |> always
                )
            ]
        , describe "Size 3 Tetromino"
            [ test "not rotated"
                (GameGrid.tetrominoes.s.cells
                    |> equal [ xy 0 1, xy 1 1, xy 1 0, xy 2 0 ]
                    |> always
                )
            , test "rotated once"
                (GameGrid.tetrominoes.s
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 1 0, xy 1 1, xy 2 1, xy 2 2 ]
                    |> always
                )
            , test "rotated twice"
                (GameGrid.tetrominoes.s
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 2 1, xy 1 1, xy 1 2, xy 0 2 ]
                    |> always
                )
            , test "rotated three times"
                (GameGrid.tetrominoes.s
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 1 2, xy 1 1, xy 0 1, xy 0 0 ]
                    |> always
                )
            , test "rotated four times"
                (GameGrid.tetrominoes.s
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> GameGrid.rotateTetromino
                    |> .cells
                    |> equal [ xy 0 1, xy 1 1, xy 1 0, xy 2 0 ]
                    |> always
                )
            ]
        ]


tetrominoMovement : Test
tetrominoMovement =
    let
        tetrominoInPlay =
            { tetromino = GameGrid.tetrominoes.i
            , position = xy 5 5
            }
    in
    describe "Tetromino Movement"
        [ test "move left"
            (tetrominoInPlay
                |> GameGrid.moveLeft
                |> .position
                |> equal (xy 4 5)
                |> always
            )
        , test "move right"
            (tetrominoInPlay
                |> GameGrid.moveRight
                |> .position
                |> equal (xy 6 5)
                |> always
            )
        , test "move down"
            (tetrominoInPlay
                |> GameGrid.moveDown
                |> .position
                |> equal (xy 5 6)
                |> always
            )
        , test "move up"
            (tetrominoInPlay
                |> GameGrid.moveUp
                |> .position
                |> equal (xy 5 4)
                |> always
            )
        ]


gameGrid : Test
gameGrid =
    let
        singleCellTetromino =
            { tetromino = Tetromino 1 GameGrid.tetrominoes.i.colour [ xy 0 0 ]
            , position = xy 5 5
            }

        multiCellTetromino =
            { tetromino = Tetromino 2 GameGrid.tetrominoes.i.colour [ xy 0 0, xy 1 0, xy 0 1, xy 1 1 ]
            , position = xy 5 5
            }

        emptyGameGrid =
            []

        nonEmptyGameGrid =
            GameGrid.mergeTetrominoInPlay
                singleCellTetromino
                []

        toGridCell : coordinate -> { cell : Cell, position : coordinate }
        toGridCell coordinate =
            { cell = Alive GameGrid.tetrominoes.i.colour
            , position = coordinate
            }
    in
    describe "GameGrid"
        [ test "merges TetrominoInPlay"
            (emptyGameGrid
                |> GameGrid.mergeTetrominoInPlay singleCellTetromino
                |> equal [ toGridCell (xy 5 5) ]
                |> always
            )
        , describe "identifies valid tetromino position"
            [ test "when tetromino is on an empty grid"
                (singleCellTetromino
                    |> GameGrid.validTetrominoPosition emptyGameGrid
                    |> equal True
                    |> always
                )
            , test "when tetromino is above occupied cell"
                (singleCellTetromino
                    |> GameGrid.moveUp
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal True
                    |> always
                )
            , test "when tetromino is below occupied cell"
                (singleCellTetromino
                    |> GameGrid.moveDown
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal True
                    |> always
                )
            , test "when tetromino is left of occupied cell"
                (singleCellTetromino
                    |> GameGrid.moveLeft
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal True
                    |> always
                )
            , test "when tetromino is right of occupied cell"
                (singleCellTetromino
                    |> GameGrid.moveRight
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal True
                    |> always
                )
            , test "when tetromino is out of bounds to the top"
                (singleCellTetromino
                    |> GameGrid.moveUp
                    |> GameGrid.moveUp
                    |> GameGrid.moveUp
                    |> GameGrid.moveUp
                    |> GameGrid.moveUp
                    |> GameGrid.moveUp
                    |> GameGrid.validTetrominoPosition emptyGameGrid
                    |> equal True
                    |> always
                )
            ]
        , describe "identifies invalid tetromino position"
            [ test "out of bounds to the left"
                (singleCellTetromino
                    |> GameGrid.moveLeft
                    |> GameGrid.moveLeft
                    |> GameGrid.moveLeft
                    |> GameGrid.moveLeft
                    |> GameGrid.moveLeft
                    |> GameGrid.moveLeft
                    |> GameGrid.validTetrominoPosition emptyGameGrid
                    |> equal False
                    |> always
                )
            , test "out of bounds to the right"
                (singleCellTetromino
                    |> GameGrid.moveRight
                    |> GameGrid.moveRight
                    |> GameGrid.moveRight
                    |> GameGrid.moveRight
                    |> GameGrid.moveRight
                    |> GameGrid.validTetrominoPosition emptyGameGrid
                    |> equal False
                    |> always
                )
            , test "collides with existing cells"
                (singleCellTetromino
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal False
                    |> always
                )
            , test "collides on tetromino cell 1"
                (multiCellTetromino
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal False
                    |> always
                )
            , test "collides on tetromino cell 2"
                (multiCellTetromino
                    |> GameGrid.moveLeft
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal False
                    |> always
                )
            , test "collides on tetromino cell 3"
                (multiCellTetromino
                    |> GameGrid.moveUp
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal False
                    |> always
                )
            , test "collides on tetromino cell 4"
                (multiCellTetromino
                    |> GameGrid.moveLeft
                    |> GameGrid.moveUp
                    |> GameGrid.validTetrominoPosition nonEmptyGameGrid
                    |> equal False
                    |> always
                )
            ]
        ]


{-| Helper function to create Coordinates.
-}
xy : Int -> Int -> Coordinate
xy =
    Coordinate
