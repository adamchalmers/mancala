module Main exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
-- import Html.Events
import List as L
import String as S


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


type State
    = Playing Game


type alias Game =
    { cells1 : List Cell
    , cells2 : List Cell
    , home1 : Cell
    , home2 : Cell
    , turn : Player
    }

type alias Cell =
    { player: Player
    , count: Int
    , kind: CellKind
    }

type CellKind = Home | Pod Int


type alias Model =
    State


modelInit : Model
modelInit =
    let
        startCount = 4
    in
    Playing
        { cells1 = L.range 1 6 |> L.map (\n -> {player = P1, count = startCount, kind = Pod n})
        , cells2 = L.range 1 6 |> L.map (\n -> {player = P2, count = startCount, kind = Pod n})
        , home1 = {player = P1, count = 0, kind = Home}
        , home2 = {player = P2, count = 0, kind = Home}
        , turn = P1
        }


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


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
            drawCellTD g.home1

        home2 =
            drawCellTD g.home2

        cells =
            table []
                [ tr [] <| L.map drawCellTD g.cells1
                , tr [] <| L.map drawCellTD g.cells2
                ]

    in
    table [ class "board outer"] [ tr [] [ home1, cells, home2 ] ]

drawCellTD : Cell -> Html msg
drawCellTD cell = 
    let
        playerClass = 
            case cell.player of
                P1 -> "p1"
                P2 -> "p2"
                
        c = 
            text <| toString cell.count

        cellKind =
            case cell.kind of
                Pod _ -> "pod"
                Home -> "home"
    in
    td [class <| S.join " " ["cell", cellKind, playerClass]] [ c ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( modelInit, Cmd.none )
