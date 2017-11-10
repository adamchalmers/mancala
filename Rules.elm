module Rules exposing (makeMove, playerCanMove, initialModel)

import Dict as D exposing (Dict)
import Maybe as M exposing (withDefault)
import List as L
import Models exposing (..)

initialModel : Model
initialModel =
    let
        startCount = 4
        numPods = 6
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

playerCanMove : Game -> Cell -> Bool
playerCanMove game cellClicked =
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
        currentCellQty = D.get (pickle cell) (game.cellQty)
        newCellQty = D.update (pickle cell) (M.map <| always 0) game.cellQty
    in
    case currentCellQty of
        Nothing -> Debug.crash <| "Couldn't find " ++ (toString cell)
        Just qty -> sowN {game | cellQty = newCellQty} (next cell) qty

sowN : Game -> Cell -> Int -> (Dict PickledCell Int, Bool)
sowN game cell n =
    let
        liftInc x = M.map (\n -> n + 1) x
        newCellQty = D.update (pickle cell) liftInc game.cellQty
    in
    case n of
        0 -> (game.cellQty, cell.kind == Pod 0)
        _ -> sowN {game | cellQty = newCellQty} (next cell) (n-1)

next : Cell -> Cell
next cell = 
    case cell.kind of
        Home -> {player = other cell.player, kind = Pod 0}
        Pod n -> 
            if n == 5 
            then {cell | kind = Home}
            else {cell | kind = Pod (n+1)}