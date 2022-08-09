(** Graphics module for our chess game.

    This module contains the sole function [show_board] which enables
    the board to be visualized in the terminal. *)

val show_board : Game.t -> Coord.t list -> unit
(** [show_board game highlighted] shows the current board state of
    [game] with squares [highlighted] highlighted *)
