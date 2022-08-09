open Coord
open Piece
open Game
open Board
open Player
open ANSITerminal

let int_to_column (i : int) : char = Char.chr (64 + i)

let get_piece_string (piece : Piece.t) : string =
  match piece.name with
  | Pawn -> "♟"
  | Rook -> "♜"
  | Queen -> "♛"
  | King -> "♚"
  | Knight -> "♞"
  | Bishop -> "♝"

let get_piece_string_outline (piece : Piece.t) : string =
  match piece.name with
  | Pawn -> "♙"
  | Rook -> "♖"
  | Queen -> "♕"
  | King -> "♔"
  | Knight -> "♘"
  | Bishop -> "♗"

let highlight_square (is_light_square : bool) =
  let bg =
    if is_light_square then Background White else Background Green
  in
  print_string [ bg; Foreground Red ] " ● "

let print_piece (piece : Piece.t option) (is_white_square : bool) =
  let bg =
    if is_white_square then Background White else Background Green
  in

  match piece with
  | Some i ->
      if i.is_white then
        print_string
          [ Foreground Black; bg ]
          (" " ^ get_piece_string_outline i ^ " ")
      else
        print_string
          [ Foreground Black; bg ]
          (" " ^ get_piece_string i ^ " ")
  | None -> print_string [ bg ] "   "

let show_board_white (game : Game.t) (highlighted : Coord.t list) =
  let board = Game.get_board game in
  for row = 8 downto 1 do
    print_string [] (string_of_int row ^ " ");
    for col = 1 to 8 do
      let coord = (int_to_column col, row) in
      let piece = Board.get_piece coord board in
      let is_white_square = (row + col) mod 2 == 1 in
      let in_highlighted =
        List.exists (fun c -> Coord.compare c coord == 0) highlighted
      in
      if in_highlighted then highlight_square is_white_square
      else print_piece piece is_white_square
    done;
    print_string [] "\n"
  done;
  print_string [] "   a  b  c  d  e  f  g  h\n"

let show_board_black (game : Game.t) (highlighted : Coord.t list) =
  let board = Game.get_board game in
  for row = 1 to 8 do
    print_string [] (string_of_int row ^ " ");
    for col = 8 downto 1 do
      let coord = (int_to_column col, row) in
      let piece = Board.get_piece coord board in
      let is_white_square = (row + col) mod 2 == 1 in
      let in_highlighted =
        List.exists (fun c -> Coord.compare c coord == 0) highlighted
      in
      if in_highlighted then highlight_square is_white_square
      else print_piece piece is_white_square
    done;
    print_string [] "\n"
  done;
  print_string [] "   h  g  f  e  d  c  b  a\n"

let rec print_pieces (pieces : Piece.t list) =
  match pieces with
  | [] -> ()
  | h :: t ->
      print_string
        [ Background Black; Foreground White ]
        (get_piece_string_outline h);
      print_pieces t

let print_graveyard (player : Player.t) (other : Player.t) =
  let pieces = Player.get_captured player in
  print_string [] "\t";
  print_pieces pieces;
  let first_points = Player.get_points player in
  let second_points = Player.get_points other in
  if first_points > second_points then (
    let diff = first_points - second_points in
    print_string [] " +";
    print_string [] (string_of_int diff);
    print_string [] "\n")
  else print_string [] "\n"

let show_board (game : Game.t) (highlighted : Coord.t list) =
  let white = fst (Game.get_players game) in
  let black = snd (Game.get_players game) in
  if Game.white_turn game then (
    print_graveyard black white;
    show_board_white game highlighted;
    print_graveyard white black)
  else (
    print_graveyard white black;
    show_board_black game highlighted;
    print_graveyard black white)
