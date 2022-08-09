(** Representation of a chess piece.

    This modules contains values for all of the possible pieces on a
    chess board. *)

type name =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King  (**[name] represents the different types of pieces in chess*)

type t = {
  is_white : bool;
  value : int;
  name : name;
  has_moved : bool option;
}
(**[t] represents a chess piece which can be either black or white and
   one of the following values: Pawn, Bishop, Knight, Rook, Queen, King*)

val wpawn : t
(**A white pawn piece*)

val bpawn : t
(**A black pawn piece*)

val wknight : t
(**A white knight piece*)

val bknight : t
(**A black knight piece*)

val wbishop : t
(**A white bishop piece*)

val bbishop : t
(**A black bishop piece*)

val wrook : t
(**A white rook piece*)

val brook : t
(**A black rook piece*)

val wqueen : t
(**A white queen piece*)

val bqueen : t
(**A black queen piece*)

val wking : t
(**A white king piece*)

val bking : t
(**A black king piece*)

val get_name_str : t -> string
(**[get_name_str piece] returns the name of [piece] as a string*)

val get_moved : t -> bool
(**[get_moved piece] returns true iff [piece] as moved and false
   otherwise*)

val set_moved : t -> bool -> t
(**[set_moved piece new_moved] is sets the has_moved field of [piece] to
   [new_moved]*)

val get_color : t -> string
(**[get_color piece] returns "black" if the piece is black and "white"
   otherwise*)
