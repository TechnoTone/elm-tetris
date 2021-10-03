module GameGridTypes exposing (..)


type alias Model =
    { current : CurrentTetromino
    , next : Tetromino
    , gridCells : List GridCell
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


type CurrentTetromino
    = NoTetromino
    | InPlay TetrominoInPlay
    | Landed TetrominoInPlay


type alias TetrominoInPlay =
    { tetromino : Tetromino
    , position : Coordinate
    }


type alias Tetromino =
    { size : Int
    , colour : Colour
    , cells : List Coordinate
    }


type alias Coordinate =
    { col : Int
    , row : Int
    }


type Colour
    = Cyan
    | Yellow
    | Magenta
    | Orange
    | Green
    | Blue
    | Red
