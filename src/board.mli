(** Representation of the chess board.

    This module represents the state of the board as the game is being
    played, including the positions of all pieces. This module also
    contains functions for validating moves, promoting pieces, and
    delivering check/checkmate. *)

type t
(**[t] represents a current board state in the chess match*)

type state =
  | Checkmate
  | Stalemate
  | Playing
      (**[state] represents the state the board is in. Either checkmate,
         stalemate, or neither *)

val new_board : t
(**[new_board] is the starting board of the chess match*)

val stalemate_test_board : t
(** [promotion_test_board] is a board state used for testing only
    (stalemate in one) *)

val get_piece : Coord.t -> t -> Piece.t option
(**[get_piece (a,b)] is the piece at (a,b), or undefined if no piece
   there*)

val update_board : Coord.t -> Coord.t -> t -> t
(**[update_board coord1 coord2 board player] moves the piece at [coord1]
   to [coord2] and returns the resultant [board]. Any pieces captured
   will be updated in [player]*)

val valid_moves : Coord.t -> t -> bool -> Coord.t list
(**[valid moves coord board white_turn] is the coordinates that the
   piece at [coord] can move given [board] *)

val find_all_valid_moves_long : t -> bool -> (Coord.t * Coord.t) list
(**[find_all_valid_moves_long board white_turn] is the set of all 
   coordinate pairs (i,j) s.t. moving the piece from [i] to [j] is 
   a valid move for the active player (white if [white_turn], black 
   if not [white_turn]) *)

val elim_color :
  Coord.t option list -> string -> t -> Coord.t option list
(**[elim_color coords color board] eliminates all coordinates in
   [coords] that have pieces of some color [color], used to restrict
   possible moves since you can't move onto a coordinate that has a
   piece with the same color as your own piece *)

val move_is_valid : Coord.t -> Coord.t -> t -> bool -> bool
(**[move_is_valid start dest board white_turn] is true iff moving the
   piece from [start] to [dest] in [board] is a valid move *)

val game_state : t -> bool -> state
(**[game_state board white_turn] returns the current state of the game.
   If there are still moves to played then the result is [Playing]. If
   there are no moves left and the king is in check, then the result is
   [Checkmate]. If there are no more moves and the king is not in check,
   then [Stalemate].*)

val is_promotable : Coord.t -> t -> bool
(**[is_promotable coord board] checks if the piece at coord [coord] is
   promotable (i.e. a pwan in the top or bottom row)*)

val promote : Coord.t -> t -> Piece.name -> t
(**[promote coord board piece] promotes the piece at coord [coord] to
   piece [piece] updating the board in the process*)
