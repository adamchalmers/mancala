module Main exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as L
import String as S
import Dict as D exposing (Dict)
import Debug
import Maybe as M exposing (withDefault)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Player
    = P1
    | P2

other : Player -> Player
other player = case player of
    P1 -> P2
    P2 -> P1

type State
    = Playing Game


type alias Game =
    { cells1 : List Cell
    , cells2 : List Cell
    , home1 : Cell
    , home2 : Cell
    , cellQty : Dict PickledCell Int
    , turn : Player
    }

type CellKind = Home | Pod Int

type alias Cell =
    { player: Player
    , kind: CellKind
    }

-- Because Elm can't put ADTs into dicts, we need an isomorphism between Cell and comparable.
type alias PickledCell = (Int, Int)

pickle : Cell -> PickledCell
pickle c = 
    let
        player = case c.player of
            P1 -> 1
            P2 -> 2
        kind = case c.kind of
            Home -> -1
            Pod n -> n
    in
    (player, kind)

unpickle : PickledCell -> Cell
unpickle (p, k) = 
    let
        player = case p of
            1 -> P1
            _ -> P2
        kind = case k of
            -1 -> Home
            n -> Pod n
    in
        {player = player, kind = kind}


type alias Model =
    State


modelInit : Model
modelInit =
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


type Msg
    = Noop
    | Click Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )
        Click cell ->
            case (model, cell.kind) of
                -- Only process clicks if the game is playing
                (Playing game, Pod _) -> 
                    if playerCanMove game cell
                    then ( Playing { game | cellQty = sow game cell, turn = other game.turn}, Cmd.none )
                    else (model, Cmd.none)
                _ -> (model, Cmd.none)

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

sow : Game -> Cell -> Dict PickledCell Int
sow game cell =
    let
        currentCellQty = D.get (pickle cell) (game.cellQty)
        newCellQty = D.update (pickle cell) (M.map <| always 0) game.cellQty
    in
    case currentCellQty of
        Nothing -> Debug.crash <| "Couldn't find " ++ (toString cell)
        Just qty -> sowN {game | cellQty = newCellQty} (next cell) qty

sowN : Game -> Cell -> Int -> Dict PickledCell Int
sowN game cell n =
    let
        liftInc x = M.map (\n -> n + 1) x
        newCellQty = D.update (pickle cell) liftInc game.cellQty
    in
    case n of
        0 -> game.cellQty
        _ -> sowN {game | cellQty = newCellQty} (next cell) (n-1)

next : Cell -> Cell
next cell = 
    case cell.kind of
        Home -> {player = other cell.player, kind = Pod 0}
        Pod n -> 
            if n == 5 
            then {cell | kind = Home}
            else {cell | kind = Pod (n+1)}


view : Model -> Html Msg
view model =
    let
        headings =
            [ h1 [] [ text "Mancala" ] ]

        gfx model =
            case model of
                Playing g ->
                    [ drawBoard g
                    , infoText g ]
    in
    div [] <| headings ++ gfx model

infoText g = text <| "It's " ++ (toString g.turn) ++ "'s turn."

drawBoard : Game -> Html Msg
drawBoard g =
    let
        home1 =
            drawCellTD g.cellQty g.home1

        home2 =
            drawCellTD g.cellQty g.home2

        cells =
            table []
                [ tr [] <| L.map (drawCellTD g.cellQty) <| L.reverse g.cells1
                , tr [] <| L.map (drawCellTD g.cellQty) g.cells2
                ]

    in
    table [ class "board outer"] [ tr [] [ home1, cells, home2 ] ]

drawCellTD : Dict PickledCell Int -> Cell -> Html Msg
drawCellTD cellQty cell = 
    let
        playerClass = 
            case cell.player of
                P1 -> "p1"
                P2 -> "p2"

        countText = 
            D.get (pickle cell) cellQty |> M.map toString |> withDefault "?"

        cellKind =
            case cell.kind of
                Pod _ -> "pod"
                Home -> "home"
        msg = onClick (Click cell)
        classes = S.join " " ["cell", cellKind, playerClass]
    in
    td [class classes, onClick (Click cell)] [ text countText ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( modelInit, Cmd.none )
