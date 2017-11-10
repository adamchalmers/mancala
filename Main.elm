module Main exposing (..)

import Html exposing (..)
import Models exposing (..)
import Rules
import GUI

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = GUI.view
        , update = update
        , subscriptions = subscriptions
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click cell ->
            case (model, cell.kind) of
                -- Only process clicks if the game is playing
                (Playing game, Pod _) -> 
                    if Rules.validMove game cell
                    then ( Playing (Rules.makeMove game cell), Cmd.none )
                    else (model, Cmd.none)
                _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Rules.initialModel, Cmd.none )
