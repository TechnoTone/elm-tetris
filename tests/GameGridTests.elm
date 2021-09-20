module GameGridTests exposing (..)

import Expect exposing (equal)
import Fuzz exposing (int, list)
import GameGrid
    exposing
        ( CurrentTetromino(..)
        , GameGridModel(..)
        , Tetromino
        )
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
        [ test "not rotated"
            (GameGrid.tetrominoes.i.cells
                |> equal [ xy 0 1, xy 1 1, xy 2 1, xy 3 1 ]
                |> always
            )
        , test "rotated once"
            (GameGrid.tetrominoes.i.cells
                |> GameGrid.rotateTetrominoCells
                |> equal [ xy 2 0, xy 2 1, xy 2 2, xy 2 3 ]
                |> always
            )
        , test "rotated twice"
            (GameGrid.tetrominoes.i.cells
                |> GameGrid.rotateTetrominoCells
                |> GameGrid.rotateTetrominoCells
                |> equal [ xy 3 2, xy 2 2, xy 1 2, xy 0 2 ]
                |> always
            )
        , test "rotated three times"
            (GameGrid.tetrominoes.i.cells
                |> GameGrid.rotateTetrominoCells
                |> GameGrid.rotateTetrominoCells
                |> GameGrid.rotateTetrominoCells
                |> equal [ xy 1 3, xy 1 2, xy 1 1, xy 1 0 ]
                |> always
            )
        , test "rotated four times"
            (GameGrid.tetrominoes.i.cells
                |> GameGrid.rotateTetrominoCells
                |> GameGrid.rotateTetrominoCells
                |> GameGrid.rotateTetrominoCells
                |> GameGrid.rotateTetrominoCells
                |> equal [ xy 0 1, xy 1 1, xy 2 1, xy 3 1 ]
                |> always
            )
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

        toGridCell : coordinate -> { cell : GameGrid.Cell, position : coordinate }
        toGridCell coordinate =
            { cell = GameGrid.Alive GameGrid.tetrominoes.i.colour
            , position = coordinate
            }
    in
    describe "GameGrid"
        [ test "merges TetrominoInPlay"
            ([]
                |> GameGrid.mergeTetrominoInPlay singleCellTetromino
                |> equal (List.map toGridCell [ xy 5 5 ])
                |> always
            )
        , describe "identifies valid tetromino position"
            [ test "when tetromino is on the grid"
                ([]
                    |> GameGrid.validTetrominoPosition singleCellTetromino
                    |> equal True
                    |> always
                )
            , test "when tetromino is out of bounds to the top"
                ([]
                    |> GameGrid.validTetrominoPosition
                        (singleCellTetromino
                            |> GameGrid.moveUp
                            |> GameGrid.moveUp
                            |> GameGrid.moveUp
                            |> GameGrid.moveUp
                            |> GameGrid.moveUp
                            |> GameGrid.moveUp
                        )
                    |> equal True
                    |> always
                )
            ]
        , describe "identifies invalid tetromino position"
            [ test "out of bounds to the left"
                ([]
                    |> GameGrid.validTetrominoPosition
                        (singleCellTetromino
                            |> GameGrid.moveLeft
                            |> GameGrid.moveLeft
                            |> GameGrid.moveLeft
                            |> GameGrid.moveLeft
                            |> GameGrid.moveLeft
                            |> GameGrid.moveLeft
                        )
                    |> equal False
                    |> always
                )
            , test "out of bounds to the right"
                ([]
                    |> GameGrid.validTetrominoPosition
                        (singleCellTetromino
                            |> GameGrid.moveRight
                            |> GameGrid.moveRight
                            |> GameGrid.moveRight
                            |> GameGrid.moveRight
                            |> GameGrid.moveRight
                        )
                    |> equal False
                    |> always
                )
            , test "collides with existing cells"
                ([]
                    |> GameGrid.mergeTetrominoInPlay singleCellTetromino
                    |> GameGrid.validTetrominoPosition singleCellTetromino
                    |> equal False
                    |> always
                )
            ]
        ]


{-| Helper function to create GameGrid Coordinates.
-}
xy : Int -> Int -> { col : Int, row : Int }
xy x y =
    { col = x, row = y }
