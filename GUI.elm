module GUI exposing (view)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as L
import Dict as D exposing (Dict)
import Maybe as M exposing (withDefault)
import String as S

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