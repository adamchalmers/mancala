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
                Winner g player ->
                    [ drawBoard g ] ++ (winnerGUI g player)
    in
    div [] <| headings ++ gfx model

infoText g = span [] 
    [ text "It's "
    , span [class <| (playerClass g.turn) ++ "-dark"] [text (toString g.turn)]
    , text "'s turn."
    ]

winnerGUI : Game -> Maybe Player -> List (Html Msg)
winnerGUI g player = 
    let
        c = withDefault "" <| M.map playerClass player
        winnerText = M.map playerString player 
            |> M.andThen (\t -> Just <| span [] 
                [ span [class <| c] [text t]
                , text " won!"
                ])
            |> withDefault (text "It's a tie!")
    in
        [ h2 [] [ winnerText ]
        , button [onClick Restart] [text "Play again"]]

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
        countText = 
            D.get (pickle cell) cellQty |> M.map toString |> withDefault "?"

        cellKind =
            case cell.kind of
                Pod _ -> "pod"
                Home -> "home"
        msg = onClick (Click cell)
        classes = S.join " " ["cell", cellKind, playerClass cell.player]
    in
    td [class classes, onClick (Click cell)] [ text countText ]


playerClass player = 
    case player of
        P1 -> "p1"
        P2 -> "p2"