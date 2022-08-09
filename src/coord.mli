(** Representation of a square on the chess board.

    This module represents a particular square on the chess board
    denoted by it's rank (1-8) and file (A-H). This module also contains
    functions for finding adjacent squares of the current square.*)

exception OutOfBounds
(**Raised when [coord] is not a valid square on the board*)

type t = char * int
(**[t] represents a particular square on the chess boards. Values range
   from A1 to H8*)

val from_string : string -> t
(**[from_string s] is the coordinate of string s *)

val string_of : t -> string
(**[string_of coord] is the string representation of [coord] *)

val string_of_list : t list -> string
(**[string_of_list coords] returns the string representation of the list
   of coords in [coords]*)

val compare : t -> t -> int
(**[compare x y] returns -1 if x comes before y, 1 if x comes after y,
   and zero if they are equal. Coords are compared first alphabetically
   and then numerically. Example: [compare A8 H1] is -1 and
   [compare B6 B3] is 1 *)

val get_col : t -> char
(**[get_col coord] returns the column of [coord] (between A-H)*)

val get_row : t -> int
(**[get_row coord] returns the row of [coord] (between 1-8)*)

val get_color : t -> string
(**[get_color coord] is the "dark" if the square is dark on the chess
   board and "light" otherwise (helps create checkerboard pattern)*)

val north : t -> t
(**[north coord] returns the square directly above [coord]. Raises:
   OutOfBounds exception if the square above is not valid*)

val south : t -> t
(**[south coord] returns the square directly below [coord]. Raises:
   OutOfBounds exception if the square below is not valid*)

val west : t -> t
(**[west coord] returns the square directly to the left of [coord].
   Raises: OutOfBounds exception if the square to the left is not valid*)

val east : t -> t
(**[east coord] returns the square directly to the right of [coord].
   Raises: OutOfBounds exception if the square to the right is not valid*)

val northwest : t -> t
(**[northwest coord] returns the square to the left and up from [coord].
   Raises: OutOfBounds exception if this square is not valid*)

val northeast : t -> t
(**[northeast coord] returns the square to the right and up from
   [coord]. Raises: OutOfBounds exception if this square is not valid*)

val southwest : t -> t
(**[southwest coord] returns the square to the left and down from
   [coord]. Raises: OutOfBounds exception if this square is not valid*)

val southeast : t -> t
(**[southeast coord] returns the square to the right and down from
   [coord]. Raises: OutOfBounds exception if this square is not valid*)

val north_opt : t -> t option
(**[north_opt coord] returns Some (square directly above [coord]) if
   valid, else None. *)

val south_opt : t -> t option
(**[south_opt coord] returns Some (square directly below [coord]) if
   valid, else None. *)

val west_opt : t -> t option
(**[west_opt coord] returns Some (square directly left of [coord]) if
   valid, else None. *)

val east_opt : t -> t option
(**[east_opt coord] returns Some (square directly right of [coord]) if
   valid, else None. *)

val northwest_opt : t -> t option
(**[northwest_opt coord] returns Some (square directly up and left of
   [coord]) if valid, else None. *)

val northeast_opt : t -> t option
(**[northeast_opt coord] returns Some (square directly up and right of
   [coord]) if valid, else None. *)

val southwest_opt : t -> t option
(**[southwest_opt coord] returns Some (square directly down and left of
   [coord]) if valid, else None. *)

val southeast_opt : t -> t option
(**[southeast_opt coord] returns Some (square directly down and right of
   [coord]) if valid, else None. *)

val north_unpr : t -> t
(**[north_unpr coord] returns square directly above [coord], even if not
   valid. *)

val south_unpr : t -> t
(**[south_unpr coord] returns square directly below [coord], even if not
   valid. *)

val west_unpr : t -> t
(**[west_unpr coord] returns square directly left of [coord], even if
   not valid. *)

val east_unpr : t -> t
(**[east_unpr coord] returns square directly right of [coord], even if
   not valid. *)

val northwest_unpr : t -> t
(**[northwest_unpr coord] returns square directly up and left of
   [coord], even if not valid. *)

val northeast_unpr : t -> t
(**[northeast_unpr coord] returns square directly up and right of
   [coord], even if not valid. *)

val southwest_unpr : t -> t
(**[southwest_unpr coord] returns square directly down and left of
   [coord], even if not valid. *)

val southeast_unpr : t -> t
(**[southeast_unpr coord] returns square directly down and right of
   [coord], even if not valid. *)
