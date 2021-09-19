module GameGridTests exposing (..)

import Expect exposing (equal)
import Fuzz exposing (int, list)
import GameGrid
    exposing
        ( Colour(..)
        , CurrentTetromino(..)
        , GameGridModel(..)
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
        [ test "default" (GameGrid.tetrominoes.i.cells |> equal [ xy 0 1, xy 1 1, xy 2 1, xy 3 1 ] |> always)
        ]


{-| Helper function to create GameGrid Coordinates.
-}
xy : Int -> Int -> GameGrid.Coordinate
xy =
    GameGrid.Coordinate
