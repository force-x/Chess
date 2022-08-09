open Sys

type name =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King

type t = {
  is_white : bool;
  value : int;
  name : name;
  has_moved : bool option;
}

(*Pre-defined pieces below*)
let wpawn =
  { is_white = true; value = 1; name = Pawn; has_moved = Some false }

let bpawn = { wpawn with is_white = false }

let wknight =
  { is_white = true; value = 3; name = Knight; has_moved = None }

let bknight = { wknight with is_white = false }

let wbishop =
  { is_white = true; value = 3; name = Bishop; has_moved = None }

let bbishop = { wbishop with is_white = false }

let wrook =
  { is_white = true; value = 5; name = Rook; has_moved = None }

let brook = { wrook with is_white = false }

let wqueen =
  { is_white = true; value = 9; name = Queen; has_moved = None }

let bqueen = { wqueen with is_white = false }

let wking =
  { is_white = true; value = 0; name = King; has_moved = None }

let bking = { wking with is_white = false }

let get_name_str (piece : t) =
  match piece.name with
  | Knight -> "knight"
  | Pawn -> "pawn"
  | Queen -> "queen"
  | Rook -> "rook"
  | Bishop -> "bishop"
  | King -> "king"

let get_moved (piece : t) = Option.value piece.has_moved ~default:false

let set_moved (piece : t) (value : bool) =
  { piece with has_moved = Some value }

let get_color (piece : t) = if piece.is_white then "white" else "black"
