-- Tetris rules being used are the SRS rules (Super Rotation System):
-- https://tetris.fandom.com/wiki/SRS


module GameGrid exposing
    ( Block(..)
    , Cell(..)
    , Column
    , Coordinate
    , Model
    , NextTetromino
    , checkForDeadCells
    , collapse
    , dropToBottom
    , eliminateCell
    , falling
    , hasDeadCells
    , hasNext
    , hasNoNext
    , height
    , init
    , isCollapsible
    , moveDown
    , moveLeft
    , moveRight
    , removeDeadCell
    , rotateLeft
    , rotateRight
    , spawnNewBlocks
    , spawningBlocked
    , view
    , width
    )

import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (int)



{-
   @@   @@  @@@@  @@@@@  @@@@@@ @@
   @@@ @@@ @@  @@ @@  @@ @@     @@
   @@ @ @@ @@  @@ @@  @@ @@@@   @@
   @@   @@ @@  @@ @@  @@ @@     @@
   @@   @@  @@@@  @@@@@  @@@@@@ @@@@@@
-}


type alias Model =
    { next : Maybe NextTetromino
    , columns : Array Column
    }


type alias NextTetromino =
    { tetromino : Tetromino
    , coordinate : Coordinate
    }


type alias Tetromino =
    { blockType : Block
    }


type alias Column =
    Array Cell


type Cell
    = Empty
    | AliveBlock Block
    | DeadBlock Block


type Block
    = Red
    | Green
    | Blue
    | Cyan
    | Yellow
    | Magenta
    | Orange


type alias Coordinate =
    { col : Int
    , row : Int
    }


width : number
width =
    10


height : Int
height =
    24


init : Model
init =
    Empty
        |> Array.repeat height
        |> Array.repeat width
        |> Model Nothing



{-
   @@  @@ @@@@@@ @@@@@@ @@   @@
   @@  @@   @@   @@     @@   @@
   @@  @@   @@   @@@@   @@ @ @@
    @@@@    @@   @@     @@@@@@@
     @@   @@@@@@ @@@@@@  @@ @@
-}


view : Model -> (Coordinate -> msg) -> Html msg
view ({ next } as gameGrid) deadCellAnimationEndMsg =
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

        captureAnimationEnd : Coordinate -> List (Html.Attribute msg)
        captureAnimationEnd coordinate =
            [ "webkitAnimationEnd", "oanimationend", "msAnimationEnd", "animationend" ]
                |> List.map
                    (\event ->
                        on event (Decode.succeed (deadCellAnimationEndMsg coordinate))
                    )

        deadCellAnimationHook : Cell -> Coordinate -> List (Html.Attribute msg)
        deadCellAnimationHook cell coordinate =
            case cell of
                DeadBlock _ ->
                    captureAnimationEnd coordinate

                _ ->
                    []

        cellClass cell =
            case cell of
                Empty ->
                    "cell_empty"

                AliveBlock block ->
                    "cell_" ++ blockClass block

                DeadBlock block ->
                    "cell_dead cell_" ++ blockClass block

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
            -- case next of
            -- Nothing ->
            getCell gameGrid (Coordinate x y)

        -- Just { blockSet, coordinate } ->
        --     if x == coordinate.col && y >= coordinate.row - 2 && y <= coordinate.row then
        --         if y + 2 == coordinate.row then
        --             AliveBlock blockSet.b1
        --         else if y + 1 == coordinate.row then
        --             AliveBlock blockSet.b2
        --         else
        --             AliveBlock blockSet.b3
        --     else
        --         getCell gameGrid (Coordinate x y)
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


spawnNewBlocks : Int -> Model -> Model
spawnNewBlocks millis model =
    model


falling : Model -> Model
falling model =
    case model.next of
        Nothing ->
            model

        Just next ->
            let
                { col, row } =
                    next.coordinate

                cellBelow =
                    if row == height - 1 then
                        AliveBlock Red

                    else
                        model.columns |> Array.get col |> Maybe.andThen (Array.get (row + 1)) |> Maybe.withDefault Empty
            in
            case cellBelow of
                Empty ->
                    updateNextBlock (updateCoordinate (updateCoordinateRow ((+) 1))) model

                _ ->
                    landNextBlock next model


setNextBlock : NextTetromino -> Model -> Model
setNextBlock nextBlock model =
    { model | next = Just nextBlock }


updateNextBlock : (NextTetromino -> NextTetromino) -> Model -> Model
updateNextBlock fn model =
    case model.next of
        Nothing ->
            model

        Just next ->
            setNextBlock (fn next) model


updateCoordinate : (Coordinate -> Coordinate) -> NextTetromino -> NextTetromino
updateCoordinate fn next =
    { next | coordinate = fn next.coordinate }


updateCoordinateRow : (Int -> Int) -> Coordinate -> Coordinate
updateCoordinateRow fn coordinate =
    { coordinate | row = fn coordinate.row }


updateCoordinateCol : (Int -> Int) -> Coordinate -> Coordinate
updateCoordinateCol fn coordinate =
    { coordinate | col = fn coordinate.col }


clearNextBlock : Model -> Model
clearNextBlock model =
    { model | next = Nothing }


eliminateCell : Coordinate -> Model -> Model
eliminateCell coordinate model =
    model |> updateCell coordinate (toEliminatedBlock (getCell model coordinate))


removeDeadCell : Coordinate -> Model -> Model
removeDeadCell coordinate model =
    model |> updateCell coordinate Empty


collapse : Model -> Model
collapse model =
    let
        collapseColumn : Column -> Column
        collapseColumn column =
            let
                l =
                    column |> Array.toList

                n =
                    column |> bottomEmptyCell
            in
            if n > 0 then
                Empty
                    :: List.take n l
                    ++ List.drop (n + 1) l
                    |> Array.fromList

            else
                column
    in
    model |> setColumns (model.columns |> Array.map collapseColumn)


toEliminatedBlock : Cell -> Cell
toEliminatedBlock aliveCell =
    case aliveCell of
        Empty ->
            Empty

        AliveBlock block ->
            DeadBlock block

        DeadBlock _ ->
            aliveCell


updateCell : Coordinate -> Cell -> Model -> Model
updateCell { col, row } cell model =
    model
        |> (Array.set col
                (Array.get col model.columns
                    |> Maybe.withDefault Array.empty
                    |> Array.set row cell
                )
                model.columns
                |> setColumns
           )


setColumns : Array Column -> Model -> Model
setColumns columns model =
    { model | columns = columns }


dropToBottom : Model -> Model
dropToBottom model =
    let
        colCells : Int -> Array Cell
        colCells col =
            model.columns |> Array.get col |> Maybe.withDefault Array.empty

        newCoordinate : Coordinate -> Coordinate
        newCoordinate coordinate =
            { coordinate | row = bottomEmptyCell <| colCells coordinate.col }
    in
    updateNextBlock (\nb -> { nb | coordinate = newCoordinate nb.coordinate }) model


rotateLeft : Model -> Model
rotateLeft model =
    -- TODO fix this
    --let
    --    update : BlockTriple -> BlockTriple
    --    update blockSet =
    --        BlockTriple blockSet.b2 blockSet.b3 blockSet.b1
    --in
    --updateNextBlock
    --    (\nb -> { nb | blockSet = update nb.blockSet })
    model


rotateRight : Model -> Model
rotateRight model =
    -- TODO fix this
    --let
    --    update : BlockTriple -> BlockTriple
    --    update blockSet =
    --        BlockTriple blockSet.b3 blockSet.b1 blockSet.b2
    --in
    --updateNextBlock
    --    (\nb -> { nb | blockSet = update nb.blockSet })
    model


moveNextBlock : Model -> Int -> NextTetromino -> NextTetromino
moveNextBlock model col nb =
    case model.next of
        Just next ->
            let
                newCoordinate =
                    Coordinate col next.coordinate.row
            in
            if
                (col >= 0)
                    && (col <= (width - 1))
                    && cellIsEmpty (getCell model newCoordinate)
            then
                { nb | coordinate = newCoordinate }

            else
                nb

        Nothing ->
            nb


moveLeft : Model -> Model
moveLeft model =
    updateNextBlock
        (\nb -> moveNextBlock model (nb.coordinate.col - 1) nb)
        model


moveRight : Model -> Model
moveRight model =
    updateNextBlock
        (\nb -> moveNextBlock model (nb.coordinate.col + 1) nb)
        model


moveDown : Model -> Model
moveDown model =
    --    TODO fix this
    model


landNextBlock : NextTetromino -> Model -> Model
landNextBlock next model =
    model
        -- |> setColumns (Array.set col column model.columns)
        |> clearNextBlock



{-
    @@@@  @@@@@@ @@@@@@ @@@@@@ @@@@@@ @@@@@   @@@@
   @@     @@       @@     @@   @@     @@  @@ @@
   @@ @@@ @@@@     @@     @@   @@@@   @@@@@   @@@@
   @@  @@ @@       @@     @@   @@     @@  @@     @@
    @@@@  @@@@@@   @@     @@   @@@@@@ @@  @@  @@@@
-}


getCell : Model -> Coordinate -> Cell
getCell { columns } { col, row } =
    columns
        |> Array.get col
        |> Maybe.andThen (Array.get row)
        |> Maybe.withDefault Empty


cellIsEmpty : Cell -> Bool
cellIsEmpty cell =
    cell == Empty


cellIsAlive : Cell -> Bool
cellIsAlive cell =
    case cell of
        AliveBlock _ ->
            True

        _ ->
            False


cellIsDead : Cell -> Bool
cellIsDead cell =
    case cell of
        DeadBlock _ ->
            True

        _ ->
            False


hasNext : Model -> Bool
hasNext { next } =
    next /= Nothing


hasNoNext : Model -> Bool
hasNoNext =
    not << hasNext


spawningBlocked : Model -> Bool
spawningBlocked model =
    -- model.columns
    --     |> Array.get (model.width // 2)
    --     |> Maybe.andThen (Array.get 0)
    --     |> Maybe.map (always True)
    --     |> Maybe.withDefault False
    False


checkForDeadCells : Model -> List Coordinate
checkForDeadCells model =
    []


hasDeadCells : Model -> Bool
hasDeadCells model =
    model.columns
        |> Array.toList
        |> List.concatMap Array.toList
        |> List.any cellIsDead


bottomEmptyCell : Array Cell -> Int
bottomEmptyCell cells =
    cells
        |> Array.toIndexedList
        |> List.filter (Tuple.second >> cellIsEmpty)
        |> List.map Tuple.first
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0


isCollapsible : Model -> Bool
isCollapsible model =
    let
        topAliveCell : Array Cell -> Int
        topAliveCell cells =
            cells
                |> Array.toIndexedList
                |> List.filter (Tuple.second >> cellIsAlive)
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault 99
    in
    model.columns
        |> Array.map (\cells -> bottomEmptyCell cells > topAliveCell cells)
        |> Array.toList
        |> List.foldr (||) False
