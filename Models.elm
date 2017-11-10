module Models exposing (..)

import Dict as D exposing (Dict)

type Player
    = P1
    | P2

other : Player -> Player
other player = case player of
    P1 -> P2
    P2 -> P1

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


type State
    = Playing Game
type alias Model =
    State

type Msg
    = Click Cell
