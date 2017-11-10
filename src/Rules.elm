module Rules exposing (validMove, initialModel, stateAfterMove)

import Dict as D exposing (Dict)
import Maybe as M exposing (Maybe, withDefault)
import List as L
import Models exposing (..)

numPods = 2
startCount = 2

qtyOf : Game -> Cell -> Maybe Int
qtyOf game cell = D.get (pickle cell) game.cellQty

initialModel : Model
initialModel =
    let
        podsFor player = L.map (\n -> {player = player, kind = Pod n}) (L.range 0 (numPods - 1))
        homeFor player = {player = player, kind = Home}
        allCells = L.concat [podsFor P1, podsFor P2, [homeFor P1, homeFor P2]]
        startingQty c = case c.kind of 
            Home -> 0 -- Homes start off empty
            Pod _ -> startCount
    in
    Playing
        { cells1 = podsFor P1
        , cells2 = podsFor P2
        , home1 = homeFor P1
        , home2 = homeFor P2
        , turn = P1
        , cellQty = D.fromList <| L.map (\c -> (pickle c, startingQty c)) allCells
        }

validMove : Game -> Cell -> Bool
validMove game cellClicked =
    let
        pos n = n > 0
        positiveQty cellQty cell = qtyOf game cell |> M.map pos |> withDefault False
    in
    L.all identity 
        [ game.turn == cellClicked.player -- Only accept moves if it's the player's turn
        , positiveQty game.cellQty cellClicked -- Only take pieces from a pod if the pod has pieces
        ]

makeMove : Game -> Cell -> (Game, Bool)
makeMove game cell = 
    let
        (cellQty, result) = sow game cell
        turn = if result == InHome then game.turn else other game.turn
    in
        ({ game | cellQty = cellQty, turn = turn}, result == GameOver)

sow : Game -> Cell -> (Dict PickledCell Int, MoveResult)
sow game cell =
    let
        -- Find how many seeds are in the current cell
        qty = case qtyOf game cell of
            Nothing -> Debug.crash <| "Couldn't find cell " ++ (toString cell)
            Just q -> q
        -- Empty the current cell
        newCellQty = D.update (pickle cell) (M.map <| always 0) game.cellQty
            
    in
    sowN {game | cellQty = newCellQty} (next game.turn cell) qty

sowN : Game -> Cell -> Int -> (Dict PickledCell Int, MoveResult)
-- Starting from `cell`, take `n` seeds and sow them in consecutive cells around the board.
-- Returns the new quantities of seeds in each cell, and whether the last cell was your home.
sowN game cell n =
    let
        liftInc x = M.map (\n -> n + 1) x
        newCellQty = D.update (pickle cell) liftInc game.cellQty
        result =
            if gameOver game
            then GameOver
            else
                if cell.kind == Pod 0
                then InHome
                else Normal
    in
    case n of
        0 -> (game.cellQty, result)
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

total : Game -> List Cell -> Int
total game = L.map (withDefault 0 << qtyOf game) >> L.sum

gameOver : Game -> Bool
-- If the game is over, return the winner. If game's ongoing, return Nothing.
gameOver game = (total game game.cells1 == 0) || (total game game.cells2 == 0)

winner : Game -> Maybe Player
-- Just player if there's a winner, Nothing if a draw.
winner game =
    let
        grandTotal player cells = (total game cells) + withDefault 0 (qtyOf game {player = player, kind = Home})
        p1Total = Debug.log "p1Total" <| grandTotal P1 game.cells1
        p2Total = Debug.log "p2Total" <| grandTotal P2 game.cells2
    in
    if p1Total > p2Total
    then Just P1
    else 
        if p2Total > p1Total
        then Just P2
        else Nothing

stateAfterMove : Game -> Cell -> State
stateAfterMove game cell = 
    let
        (gameAfterMove, result) = makeMove game cell
    in
    case result of
        True -> Winner gameAfterMove (winner gameAfterMove)
        _ -> Playing gameAfterMove