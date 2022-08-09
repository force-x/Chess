type t = {
  board : Board.t;
  players : Player.t * Player.t;
  move_history :
    ((Piece.t * Piece.t option) * (Coord.t * Coord.t)) list;
  white_turn : bool;
}

let new_game =
  {
    board = Board.new_board;
    players = (Player.new_player, Player.new_player);
    move_history = [];
    white_turn = true;
  }

let get_board game = game.board
let get_players game = game.players
let white_turn game = game.white_turn

let get_active_player game =
  if game.white_turn then fst game.players else snd game.players

let update_game (game : t) (new_board : Board.t) =
  { game with board = new_board; white_turn = not game.white_turn }

let update_board (game : t) (new_board : Board.t) : t =
  { game with board = new_board }

let update_game_move
    (coord1 : Coord.t)
    (coord2 : Coord.t)
    (curr_game : t) =
  let curr_board = curr_game.board in
  let piece1_option = Board.get_piece coord1 curr_board in
  if not (Option.is_none piece1_option) then
    let piece1 = Option.get piece1_option in
    let piece2_option = Board.get_piece coord2 curr_board in
    let new_board = Board.update_board coord1 coord2 curr_board in
    if not (Option.is_none piece2_option) then
      let piece2 = Option.get piece2_option in
      let new_player =
        get_active_player curr_game |> Player.add_to_pieces piece2
      in
      let new_players =
        if curr_game.white_turn then (new_player, snd curr_game.players)
        else (fst curr_game.players, new_player)
      in
      {
        board = new_board;
        players = new_players;
        move_history =
          curr_game.move_history
          @ [ ((piece1, Some piece2), (coord1, coord2)) ];
        white_turn = not curr_game.white_turn;
      }
    else
      {
        curr_game with
        board = new_board;
        move_history =
          curr_game.move_history
          @ [ ((piece1, None), (coord1, coord2)) ];
        white_turn = not curr_game.white_turn;
      }
  else curr_game

let coord_is_active_piece (coord : Coord.t) (game : t) : bool =
  let white_move = white_turn game in
  let piece_opt = game |> get_board |> Board.get_piece coord in
  match piece_opt with
  | None -> false
  | Some i -> i.is_white == white_move

let string_of_piece (p : Piece.t) : string =
  Piece.get_color p ^ " " ^ Piece.get_name_str p

let history_pair_to_str_new
    (coord_pair : (Piece.t * Piece.t option) * (Coord.t * Coord.t)) :
    string =
  let end_coord = snd (snd coord_pair) in
  let did_capture = coord_pair |> fst |> snd |> Option.is_some in
  let coord_start = coord_pair |> snd |> fst in
  let piece = coord_pair |> fst |> fst in
  match piece.name with
  | Pawn ->
      if did_capture then
        let file =
          String.make 1 (fst coord_start) |> String.lowercase_ascii
        in
        file ^ "x" ^ Coord.string_of end_coord
      else Coord.string_of end_coord
  | _ ->
      let piece_char =
        match piece.name with
        | Queen -> "Q"
        | Rook -> "R"
        | Bishop -> "B"
        | King -> "K"
        | Knight -> "N"
        | Pawn -> "P"
      in
      if did_capture then piece_char ^ "x" ^ Coord.string_of end_coord
      else piece_char ^ Coord.string_of end_coord

let history_pair_to_str
    (coord_pair : (Piece.t * Piece.t option) * (Coord.t * Coord.t)) :
    string =
  let capt_opt =
    if snd (fst coord_pair) <> None then
      " captured " ^ string_of_piece (Option.get (snd (fst coord_pair)))
    else ""
  in
  let str =
    "moved "
    ^ string_of_piece (fst (fst coord_pair))
    ^ " from "
    ^ Coord.string_of (fst (snd coord_pair))
    ^ " to "
    ^ Coord.string_of (snd (snd coord_pair))
    ^ capt_opt
  in
  String.trim str

let rec history_parse
    (hist : ((Piece.t * Piece.t option) * (Coord.t * Coord.t)) list) :
    string =
  match hist with
  | [] -> ""
  | h :: t -> history_pair_to_str_new h ^ "\n" ^ history_parse t

let history_string (game : t) : string = history_parse game.move_history
