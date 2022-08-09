(** Representation of the chess game itself.

    This module represents the state of the current game. It includes
    the current board, the move history thus far, and also the current
    players. Functions are included which allow the individual elements
    of the game's state to be updated. *)

type t
(**[t] represents the chess match as a whole. It manages the current
   board state, active players, and move history.*)

val new_game : t
(** [new_game] represents a new game (initial board setup, reset
    players, etc) *)

val update_game : t -> Board.t -> t
(** [update_game game board] updates the board of [game] to [board] and
    changes who's turn it is*)

val update_board : t -> Board.t -> t
(** [update_board game board] updates the board of [game] to [board] *)

val update_game_move : Coord.t -> Coord.t -> t -> t
(** [update_game_move start end game] puts the piece at [start] to [end]
    in [game], adding captured pieces accordingly *)

val get_board : t -> Board.t
(** [get_board game] returns the current board of [game] *)

val white_turn : t -> bool
(** [white_turn game] is true if it's currently white to move in [game]*)

val get_players : t -> Player.t * Player.t
(** [get_players game] returns (<white_player>, <black_player>) in
    [game]*)

val coord_is_active_piece : Coord.t -> t -> bool
(** [coord_is_active_piece coord game] returns true iff the piece at
    [coord] is the same color as the active player in [game] *)

val history_string : t -> string
(** [history_string game] creates a string to represent the move history
    of the game. It includes what pieces were captured and what pieces
    were moved *)
