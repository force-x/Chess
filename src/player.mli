(** Representation of a player of our chess game.

    This module represents the state of a player as they play the game.
    This allows us to track each player's captured pieces and overall
    score. *)

type t
(**[t] represents one of the two players in the chess match*)

val new_player : t
(**[new_player] represents a player at the start of a match*)

val add_to_pieces : Piece.t -> t -> t
(**[add_to_pieces piece player] adds [piece] to the captured pieces of
   [player]*)

val get_captured : t -> Piece.t list
(** [get_captured player] is a list of all pieces that [player] has
    captured *)

val get_points : t -> int
(** [get_points player] is the total point value of the pieces captured
    by [player] *)
