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

type alias Cell =
    { player: Player
    , kind: CellKind
    }

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
        

type CellKind = Home | Pod Int


type alias Model =
    State


modelInit : Model
modelInit =
    let
        startCount = 4
        cells1 = L.range 0 5 |> L.map (\n -> {player = P1, kind = Pod n})
        cells2 = L.range 0 5 |> L.map (\n -> {player = P2, kind = Pod n})
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
                -- Only do something if the player is playing and you press a Pod (not a Home)
                (Playing game, Pod _) -> ( Playing <| sow game cell, Cmd.none )
                _ -> (model, Cmd.none)

sow : Game -> Cell -> Game
sow game cell =
    let
        qty = D.get (pickle cell) (game.cellQty)
        newCellQty = D.update (pickle cell) (M.map <| always 0) game.cellQty
    in
    case qty of
        Nothing -> Debug.crash <| "Couldn't find " ++ (toString cell)
        Just q -> sowN {game | cellQty = newCellQty} (next cell) q

sowN : Game -> Cell -> Int -> Game
sowN game cell q =
    let
        liftInc x = M.map (\n -> n + 1) x
        newCellQty = D.update (pickle cell) liftInc game.cellQty
    in
    case q of
        0 -> game
        _ -> sowN {game | cellQty = newCellQty} (next cell) (q-1)

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
                    [ drawBoard g ]
    in
    div [] <| headings ++ gfx model


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
