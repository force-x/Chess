(** Representation of the terminal commands.

    This module represents the commands the will be inputted into the
    terminal that enables the user to play the game. Also, included in
    this module are functions that parse the user's string inputs into
    command types.*)

type command =
  | Help
  | Move of Coord.t * Coord.t
  | Moves of Coord.t
  | History
  | Show
  | Quit
  | Random
      (**[command] represents the different types of commands inputted
         into the terminal while playing*)

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

exception BadSquare
(** Raised when a bad square string is encountered. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows.

    - If the string is "help", yields the [Help] command
    - If the string is "random", plays a random valid move
    - If the string is a single square, yields [Move str]. If the square
      is malformed, raises [BadSquare]
    - If the string is two squares, yields [Moves <square> <square>]. If
      either square is malformed, raises [BadSquare]
    - If the string is more than two tokens, raised [Malformed] *)

val parse_promotion : string -> Piece.name
(**[parse_promotion str] converts the player's input into a piece name
   as follows:

   - "knight" -> Knight
   - "bishop" -> Bishop
   - "rook" -> Rook
   - "queen" -> Queen *)
