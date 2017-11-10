module Rules exposing (makeMove, validMove, initialModel)

import Dict as D exposing (Dict)
import Maybe as M exposing (withDefault)
import List as L
import Models exposing (..)

numPods = 6

initialModel : Model
initialModel =
    let
        startCount = 4
        podRange = L.range 0 (numPods - 1)
        cells1 = podRange |> L.map (\n -> {player = P1, kind = Pod n})
        cells2 = podRange |> L.map (\n -> {player = P2, kind = Pod n})
        home1 = {player = P1, kind = Home}
        home2 = {player = P2, kind = Home}
        allCells = L.concat [cells1, cells2, [home1, home2]]
    in
    Playing
        { cells1 = cells1
        , cells2 = cells2
        , home1 = home1
        , home2 = home2
        , turn = P1
        , cellQty = D.fromList <| L.map (\c -> (pickle c, startCount)) allCells
        }

validMove : Game -> Cell -> Bool
validMove game cellClicked =
    let
        pos n = n > 0
        positiveQty cellQty cell = D.get (pickle cell) cellQty |> M.map pos |> withDefault False
    in
    L.all identity 
        [ game.turn == cellClicked.player -- Only accept moves if it's the player's turn
        , positiveQty game.cellQty cellClicked -- Only take pieces from a pod if the pod has pieces
        ]

makeMove : Game -> Cell -> Game
makeMove game cell = 
    let
        (cellQty, finishedInHome) = sow game cell
        turn = if finishedInHome then game.turn else other game.turn
    in
    { game | cellQty = cellQty, turn = turn}

sow : Game -> Cell -> (Dict PickledCell Int, Bool)
sow game cell =
    let
        -- Find how many seeds are in the current cell
        currentCellQty = D.get (pickle cell) (game.cellQty)
        qty = case currentCellQty of
            Nothing -> Debug.crash <| "Couldn't find cell " ++ (toString cell)
            Just q -> q
        -- Empty the current cell
        newCellQty = D.update (pickle cell) (M.map <| always 0) game.cellQty
            
    in
    sowN {game | cellQty = newCellQty} (next game.turn cell) qty

sowN : Game -> Cell -> Int -> (Dict PickledCell Int, Bool)
-- Starting from `cell`, take `n` seeds and sow them in consecutive cells around the board.
-- Returns the new quantities of seeds in each cell, and whether the last cell was your home.
sowN game cell n =
    let
        liftInc x = M.map (\n -> n + 1) x
        newCellQty = D.update (pickle cell) liftInc game.cellQty
    in
    case n of
        0 -> (game.cellQty, cell.kind == Pod 0)
        _ -> sowN {game | cellQty = newCellQty} (next game.turn cell) (n-1)

next : Player -> Cell -> Cell
-- When `player` is sowing, returns the cell which comes after `cell`
next whoseTurn cell = 
    case cell.kind of
        Home -> {player = other cell.player, kind = Pod 0}
        Pod n -> 
            if n == numPods - 1 
            then 
                if cell.player == whoseTurn
                then {cell | kind = Home}
                else {player = other cell.player, kind = Pod 0}
            else {cell | kind = Pod (n+1)}