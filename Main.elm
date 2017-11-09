module Main exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
-- import Html.Events
import List as L


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
    , position: Int
    }


type alias Model =
    State


modelInit : Model
modelInit =
    let
        startCount = 4
    in
    Playing
        { cells1 = L.range 1 6 |> L.map (\n -> {player = P1, count = startCount, position = n})
        , cells2 = L.range 1 6 |> L.map (\n -> {player = P2, count = startCount, position = n})
        , home1 = {player = P1, count = 0, position = 7}
        , home2 = {player = P2, count = 0, position = 7}
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
            td [ class "border" ] [ text <| toString g.home1 ]

        home2 =
            td [ class "border" ] [ text <| toString g.home2 ]

        cells =
            table [ class "border" ]
                [ tr [ class "border" ] <| L.map drawCellTD g.cells1
                , tr [ class "border" ] <| L.map drawCellTD g.cells2
                ]

    in
    table [] [ tr [] [ home2, cells, home1 ] ]

drawCellTD : Cell -> Html msg
drawCellTD cell = 
    td [ class "border-light" ] [ text <| toString cell.count ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( modelInit, Cmd.none )
