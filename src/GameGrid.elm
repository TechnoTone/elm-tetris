-- Tetris rules being used are the SRS rules (Super Rotation System):
-- https://tetris.fandom.com/wiki/SRS


module GameGrid exposing
    ( GameGridModel
    , Msg
    , handleAction
    , init
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
    | Initialising { next : Tetromino }
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
    = Red
    | Green
    | Blue
    | Cyan
    | Yellow
    | Magenta
    | Orange


type alias Tetromino =
    { shape : TetrominoShape
    , orientation : Orientation
    }


type Orientation
    = R0
    | R90
    | R180
    | R270


type alias TetrominoShape =
    { width : Int
    , height : Int
    , block : Colour
    , orientations : List (List Coordinate)
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
            Initialising { next = randomTetromino millis }

        Initialising { next } ->
            Debug.todo "branch 'Initialising _' not implemented"

        Initialised model ->
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


all : { i : TetrominoShape }
all =
    { i = i
    }


i : TetrominoShape
i =
    { width = 4
    , height = 4
    , block = Red
    , orientations = []
    }


randomTetromino : Int -> Tetromino
randomTetromino seed =
    Tetromino i R0
