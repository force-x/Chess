open Chess
open ANSITerminal
open Command

let help_string =
  "<square> (ex. 'e3') — will tell you a list of squares you can move \
   the piece at that square\n\
   \t\n\
   <square1> <square2> (ex. 'e3 e4') - move a piece from <square1> to \
   <square2>\n\
   \t\n\
   history - will print the list of moves played up to this point\n\
  \  \n\
   quit - will quit the game\n\
  \  \n\
   random - will play a random valid move\n\
  \  \n\
   show - will show the board state\n\
   \t\n\
   help - a reminder of the commands\n\n"

let rec get_command () =
  print_string [ Foreground Blue ] "\n> ";
  let input = read_line () in
  if String.equal input "" then get_command ()
  else
    try
      let command = Command.parse input in
      if command = Help then (
        print_string [ Foreground Blue ] help_string;
        get_command ())
      else command
    with
    | Malformed ->
        print_string [ Foreground Red ] "Too many things\n";
        get_command ()
    | BadSquare ->
        print_string [ Foreground Red ]
          "One of those isn't a real square\n";
        get_command ()
    | Empty -> get_command ()

(*[prompt_promotion ()] prompts user until player enters one of the
  following: knight, bishop, rook, or queen *)
let rec prompt_promotion () =
  print_string [ Foreground Blue ] "\n> ";
  let input = read_line () in
  if String.equal input "" then prompt_promotion ()
  else
    try Command.parse_promotion input
    with Malformed ->
      print_string [ Foreground Red ] "Invalid piece to promote to!\n";
      prompt_promotion ()

let () =
  print_string []
    "\n\n\n\
     Welcome to CHESS.\n\
     \tAt any time, you can type 'help' to see the list of commands.\n\n";
  try
    let rec play
        (game : Game.t)
        (highlighted : Coord.t list)
        (verbose : bool) =
      match
        Board.game_state (Game.get_board game) (Game.white_turn game)
      with
      | Checkmate ->
          if Game.white_turn game then
            print_string [] "\n\nBlack wins!\n"
          else print_string [] "\n\nWhite wins!\n";
          raise Exit
      | Stalemate ->
          print_string []
            "\n\n Stalemate! No one wins! Thanks for playing! \n";
          raise Exit
      | Playing -> (
          let color_to_move =
            if Game.white_turn game then "White" else "Black"
          in
          (* print_string [] "\nHIGHLIHGTING"; print_string []
             (Coord.string_of_list highlighted); *)
          if verbose then
            print_string [] ("\n\n" ^ color_to_move ^ " to move:\n\n");
          Graphics.show_board game highlighted;
          let command = get_command () in
          match command with
          | Random ->
              Random.self_init ();
              let valid_moves =
                Board.find_all_valid_moves_long (Game.get_board game)
                  (Game.white_turn game)
              in
              let ind = Random.int (List.length valid_moves) in
              let move = List.nth valid_moves ind in
              let new_game =
                Game.update_game_move (fst move) (snd move) game
              in
              play new_game [] true
          | Help ->
              print_string [ Foreground Blue ] help_string;
              play game [] false
          | History ->
              let format_history hist =
                let rec inner moves move_number =
                  match moves with
                  | [] -> ""
                  | h1 :: h2 :: t ->
                      string_of_int move_number
                      ^ ". " ^ h1 ^ " " ^ h2 ^ "  "
                      ^ inner t (move_number + 1)
                  | [ h ] -> string_of_int move_number ^ ". " ^ h
                in
                inner (String.split_on_char '\n' hist) 1
              in
              let formatted =
                format_history (Game.history_string game) ^ "\n"
              in

              print_string [ Foreground Blue ] formatted;
              play game [] false
          | Move (i, j) ->
              if Game.coord_is_active_piece i game then
                if
                  Board.move_is_valid i j (Game.get_board game)
                    (Game.white_turn game)
                then
                  let new_game = Game.update_game_move i j game in
                  let new_board =
                    if Board.is_promotable j (Game.get_board new_game)
                    then begin
                      Graphics.show_board new_game [];
                      print_string []
                        "\n\
                         Type the piece you wish to promote to \
                         (knight, bishop, rook, queen): \n";
                      prompt_promotion ()
                      |> Board.promote j (Game.get_board new_game)
                    end
                    else Game.get_board new_game
                  in
                  let new_new_game =
                    Game.update_board new_game new_board
                  in
                  play new_new_game [] true
                else (
                  print_string [ Foreground Yellow ]
                    "\nNot a valid move.\n\n";
                  play game [] false)
              else (
                print_string [ Foreground Yellow ]
                  "\nNot a movable piece.";
                play game [] false)
          | Moves i ->
              if Game.coord_is_active_piece i game then
                let valid_moves =
                  Board.valid_moves i (Game.get_board game)
                    (Game.white_turn game)
                in
                play game valid_moves true
              else
                print_string [ Foreground Yellow ]
                  "\nNot a movable piece.";
              play game [] false
          | Quit -> raise Exit
          | Show -> play game [] true)
    in
    play Game.new_game [] true
  with Exit ->
    print_string [ Foreground Blue ] "\nThanks for playing!\n\n"
