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
    { cells1 : List Int
    , cells2 : List Int
    , home1 : Int
    , home2 : Int
    , turn : Player
    }


type alias Model =
    State


modelInit : Model
modelInit =
    Playing
        { cells1 = L.repeat 6 4
        , cells2 = L.repeat 6 4
        , home1 = 0
        , home2 = 0
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
                [ tr [ class "border" ] <| tds g.cells1
                , tr [ class "border" ] <| tds g.cells2
                ]

        tds cells =
            L.map toTD cells

        toTD n =
            td [ class "border-light" ] [ text <| toString n ]
    in
    table [] [ tr [] [ home2, cells, home1 ] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( modelInit, Cmd.none )
