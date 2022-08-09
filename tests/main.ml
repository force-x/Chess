(** TEST PLAN:

    - Our approach to testing is twofold 1) to unit test each piece and
      their interactions on the board by all possible types of
      interactions, valid and invalid. We chose this approach because
      the rules of chess are well-defined, so as long as we can test
      each rule individually, we can guarantee that the optimal behavior
      will hold for that rule no matter the board position. 2) to use
      automatic testing by randomly choosing a valid move for each
      player. We have one long game that halts after 100 moves and 5
      smaller games that halt after 20 moves. The reason for this is
      that we aren't limited to a depth-first long game, nor many
      breadth-first short games.
    - This test suite uses exclusively blackbox testing, for similar
      reasons as outlined above. Given the rules of chess for each piece
      (regardless of our specific implementation), we can construct a
      test suite that covers each rule for each piece. *)

open OUnit2
open Chess

exception InvalidMove

let make_test (name : string) (expr_str : string) (val_str : string) =
  name >:: fun _ -> assert_equal expr_str val_str ~printer:(fun x -> x)

(** [run_moves moves] simulates [moves] starting from start position
    board *)
let run_moves (moves : string list) =
  let g = Game.new_game in
  let rec inner movelist game =
    match movelist with
    | [] -> game
    | h :: t ->
        let coords = String.split_on_char ' ' h in
        let start_coord = Coord.from_string (List.nth coords 0) in
        let end_coord = Coord.from_string (List.nth coords 1) in
        if
          Board.move_is_valid start_coord end_coord
            (Game.get_board game) (Game.white_turn game)
        then
          let new_game =
            Game.update_game_move start_coord end_coord game
          in
          inner t new_game
        else raise InvalidMove
  in
  inner moves g

(** [run_moves_history name exp moves] simulates [moves] on start
    position board, then genarates val_str s.t.

    - If all [moves] are valid, val_str is the last line printed by
      moves history
    - If there's an invalid move in [moves], val_str is "invalid", then
      runs [make_test name exp val_str] *)
let run_moves_history
    (name : string)
    (expected : string)
    (moves : string list) =
  try
    let new_game = run_moves moves in
    let history = String.trim (Game.history_string new_game) in
    make_test name expected
      (history |> String.split_on_char '\n' |> List.rev |> List.hd)
  with InvalidMove -> make_test name expected "invalid"

(** [run_moves_board_state name exp moves] simulates [moves] on start
    position board, then genarates val_str s.t.

    - If all [moves] are valid, val_str is the final board state
    - If there's an invalid move in [moves], val_str is "invalid", then
      runs [make_test name exp val_str] *)
let run_moves_board_state
    (name : string)
    (expected : string)
    (moves : string list) =
  try
    let new_game = run_moves moves in
    let state =
      Board.game_state
        (Game.get_board new_game)
        (Game.white_turn new_game)
    in
    let state_str =
      match state with
      | Playing -> "playing"
      | Checkmate -> "checkmate"
      | Stalemate -> "stalemate"
    in
    make_test name expected state_str
  with InvalidMove -> make_test name expected "invalid"

(** [run_moves_board_state name game exp moves] simulates [moves] on
    start position board, then genarates val_str s.t.

    - If all [moves] are valid, val_str is the final board state
    - If there's an invalid move in [moves], val_str is "invalid", then
      runs [make_test name exp val_str] *)
let run_moves_board_state_custom_game
    (name : string)
    (game : Game.t)
    (expected : string)
    (moves : string list) =
  try
    let g = game in
    let rec inner movelist game =
      match movelist with
      | [] -> game
      | h :: t ->
          let coords = String.split_on_char ' ' h in
          let start_coord = Coord.from_string (List.nth coords 0) in
          let end_coord = Coord.from_string (List.nth coords 1) in
          if
            Board.move_is_valid start_coord end_coord
              (Game.get_board game) (Game.white_turn game)
          then
            let new_game =
              Game.update_game_move start_coord end_coord game
            in
            inner t new_game
          else raise InvalidMove
    in
    let new_game = inner moves g in
    let state =
      Board.game_state
        (Game.get_board new_game)
        (Game.white_turn new_game)
    in
    let state_str =
      match state with
      | Playing -> "playing"
      | Checkmate -> "checkmate"
      | Stalemate -> "stalemate"
    in
    make_test name expected state_str
  with InvalidMove -> make_test name expected "invalid"

let manual_tests =
  [
    (* Coordinate representation *)
    make_test "coord from string" "f5"
      (Coord.string_of (Coord.from_string "f5"));
    (* Pawn moves *)
    run_moves_history "pawn one first move" "e3" [ "e2 e3" ];
    run_moves_history "pawn two first move" "e4" [ "e2 e4" ];
    run_moves_history "pawn illegal first move" "invalid" [ "c2 c5" ];
    run_moves_history "pawn one after first valid" "e5"
      [ "e2 e4"; "a7 a6"; "e4 e5" ];
    run_moves_history "pawn two after first invalid" "invalid"
      [ "e2 e4"; "a7 a6"; "e4 e6" ];
    run_moves_history "pawn capture diagonal" "exd5"
      [ "e2 e4"; "d7 d5"; "e4 d5" ];
    run_moves_history "pawn move diagonal invalid" "invalid"
      [ "e2 e4"; "d7 d5"; "e4 f5" ];
    run_moves_history "pawn move forward blocked invalid" "invalid"
      [ "e2 e4"; "e7 e5"; "e4 e5" ];
    (* Rook moves *)
    run_moves_history "rook up" "Ra3" [ "a2 a4"; "h7 h6"; "a1 a3" ];
    run_moves_history "rook right" "Rb3"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 b3" ];
    run_moves_history "rook right far" "Rh3"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 h3" ];
    run_moves_history "rook up far" "Rg6"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 g3"; "h5 h4"; "g3 g6" ];
    run_moves_history "rook down" "Ra1"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 a1" ];
    run_moves_history "rook diagonal invalid" "invalid"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 b4" ];
    run_moves_history "rook capture far" "Rxb7"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 b3"; "h5 h4"; "b3 b7" ];
    run_moves_history "rook capture own piece invalid" "invalid"
      [ "a2 a4"; "h7 h6"; "a1 a4" ];
    run_moves_history "rook move through own piece invalid" "invalid"
      [ "a1 a3" ];
    run_moves_history "rook move through opponent piece invalid"
      "invalid"
      [ "a2 a4"; "h7 h6"; "a1 a3"; "h6 h5"; "a3 b3"; "h5 h4"; "b3 b8" ];
    (* Bishop moves *)
    run_moves_history "bishop left up" "Bc4"
      [ "e2 e4"; "a7 a6"; "f1 c4" ];
    run_moves_history "bishop right down" "Be2"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 e2" ];
    run_moves_history "bishop right up" "Be6"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 e6" ];
    run_moves_history "bishop capture" "Bxf7"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 f7" ];
    run_moves_history "bishop up invalid" "invalid"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 c5" ];
    run_moves_history "bishop down invalid" "invalid"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 c3" ];
    run_moves_history "bishop side invalid" "invalid"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 d4" ];
    run_moves_history "bishop capture own piece invalid" "invalid"
      [ "f1 e2" ];
    run_moves_history "bishop move through own piece invalid" "invalid"
      [ "f1 c4" ];
    run_moves_history "bishop move through opponent piece invalid"
      "invalid"
      [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "c4 g8" ];
    (* Queen moves *)
    run_moves_history "queen right up" "Qe2"
      [ "e2 e4"; "a7 a6"; "d1 e2" ];
    run_moves_history "queen left down" "Qd1"
      [ "e2 e4"; "a7 a6"; "d1 e2"; "a6 a5"; "e2 d1" ];
    run_moves_history "queen up" "Qe3"
      [ "e2 e4"; "a7 a6"; "d1 e2"; "a6 a5"; "e2 e3" ];
    run_moves_history "queen down" "Qe2"
      [ "e2 e4"; "a7 a6"; "d1 e2"; "a6 a5"; "e2 e3"; "a5 a4"; "e3 e2" ];
    run_moves_history "queen capture" "Qxf7"
      [ "e2 e4"; "a7 a6"; "d1 h5"; "a6 a5"; "h5 f7" ];
    run_moves_history "queen capture own piece invalid" "invalid"
      [ "d1 e2" ];
    run_moves_history "queen move through own piece invalid" "invalid"
      [ "d1 h5" ];
    run_moves_history "queen move through opponent piece invalid"
      "invalid"
      [ "e2 e4"; "a7 a6"; "d1 h5"; "a6 a5"; "h5 e8" ];
    run_moves_history "queen weird move invalid" "invalid"
      [ "e2 e4"; "a7 a6"; "d1 h5"; "a6 a5"; "h5 a2" ];
    (* Knight moves *)
    run_moves_history "knight uur" "Nh3" [ "g1 h3" ];
    run_moves_history "knight uul" "Nf3" [ "g1 f3" ];
    run_moves_history "knight ddl" "Ng1" [ "g1 f3"; "a7 a6"; "f3 g1" ];
    run_moves_history "knight ddr" "Ng1" [ "g1 h3"; "a7 a6"; "h3 g1" ];
    run_moves_history "knight capture own piece invalid" "invalid"
      [ "g1 e2" ];
    run_moves_history "knight llu" "Ne2" [ "e2 e4"; "e7 e5"; "g1 e2" ];
    (* King moves *)
    run_moves_history "king moves single" "Ke2"
      [
        "e2 e4";
        "a7 a6";
        "e1 e2";
        "a6 a5";
        "e2 d3";
        "a5 a4";
        "d3 e3";
        "a4 a3";
        "e3 e2";
      ];
    run_moves_history "king cannot move more than one" "invalid"
      [ "e2 e4"; "a7 a6"; "e1 e3" ];
    run_moves_history "king cannot move into check" "invalid"
      [ "e2 e4"; "e7 e5"; "e1 e2"; "a7 a6"; "e2 e3"; "a6 a5"; "e3 d4" ];
    run_moves_history "king cannot stay in check" "invalid"
      [ "e2 e4"; "e7 e5"; "e1 e2"; "a7 a6"; "e2 e3"; "f8 c5"; "h2 h3" ];
    (* pin to king *)
    run_moves_history "can't move piece pinned to king" "invalid"
      [ "e2 e4"; "e7 e6"; "d1 h5"; "f7 f6" ];
    (* checkmate *)
    run_moves_board_state "scholar's mate" "checkmate"
      [ "e2 e4"; "a7 a6"; "d1 h5"; "a6 a5"; "f1 c4"; "a5 a4"; "h5 f7" ];
    (*stalemate*)
    run_moves_board_state_custom_game "stalemate"
      (Game.update_board Game.new_game Board.stalemate_test_board)
      "stalemate" [ "a5 a6" ];
  ]

(** [auto_play n] randomly chooses for both the white and black players,
    halting after [n] turns for each player (2n moves total) *)

let auto_play (n : int) (name : string) =
  Random.self_init ();
  let rec inner n game =
    if n > 0 then
      let valid_moves =
        Board.find_all_valid_moves_long (Game.get_board game)
          (Game.white_turn game)
      in
      if List.length valid_moves > 0 then
        let ind = Random.int (List.length valid_moves) in
        let move = List.nth valid_moves ind in
        let new_game =
          Game.update_game_move (fst move) (snd move) game
        in
        inner (n - 1) new_game
  in

  inner (2 * n) Game.new_game;
  name >:: fun _ -> assert_string ""

let auto_tests =
  [
    auto_play 200 "very long";
    auto_play 100 "long";
    auto_play 20 "short 1";
    auto_play 20 "short 2";
    auto_play 20 "short 3";
    auto_play 20 "short 4";
    auto_play 20 "short 5";
  ]

let suite = "suite" >::: manual_tests @ auto_tests
let () = run_test_tt_main suite