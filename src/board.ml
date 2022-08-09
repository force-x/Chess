module BoardMap = Map.Make (Coord)

type t = Piece.t BoardMap.t

type state =
  | Checkmate
  | Stalemate
  | Playing

let stalemate_test_board : t =
  BoardMap.(
    empty
    |> add ('A', 8) Piece.bking
    |> add ('A', 7) { Piece.wpawn with has_moved = Some true }
    |> add ('A', 5) Piece.wking)

let new_board : t =
  BoardMap.(
    empty
    |> add ('A', 8) Piece.brook
    |> add ('B', 8) Piece.bknight
    |> add ('C', 8) Piece.bbishop
    |> add ('D', 8) Piece.bqueen
    |> add ('E', 8) Piece.bking
    |> add ('F', 8) Piece.bbishop
    |> add ('G', 8) Piece.bknight
    |> add ('H', 8) Piece.brook
    |> add ('A', 7) Piece.bpawn
    |> add ('B', 7) Piece.bpawn
    |> add ('C', 7) Piece.bpawn
    |> add ('D', 7) Piece.bpawn
    |> add ('E', 7) Piece.bpawn
    |> add ('F', 7) Piece.bpawn
    |> add ('G', 7) Piece.bpawn
    |> add ('H', 7) Piece.bpawn
    |> add ('A', 2) Piece.wpawn
    |> add ('B', 2) Piece.wpawn
    |> add ('C', 2) Piece.wpawn
    |> add ('D', 2) Piece.wpawn
    |> add ('E', 2) Piece.wpawn
    |> add ('F', 2) Piece.wpawn
    |> add ('G', 2) Piece.wpawn
    |> add ('H', 2) Piece.wpawn
    |> add ('A', 1) Piece.wrook
    |> add ('B', 1) Piece.wknight
    |> add ('C', 1) Piece.wbishop
    |> add ('D', 1) Piece.wqueen
    |> add ('E', 1) Piece.wking
    |> add ('F', 1) Piece.wbishop
    |> add ('G', 1) Piece.wknight
    |> add ('H', 1) Piece.wrook)

let get_piece (coord : Coord.t) (board : t) : Piece.t option =
  BoardMap.find_opt coord board

let update_board (coord1 : Coord.t) (coord2 : Coord.t) (curr_board : t)
    =
  let curr_piece = BoardMap.find coord1 curr_board in
  let curr_piece_moved = { curr_piece with has_moved = Some true } in
  match BoardMap.find coord2 curr_board with
  | p ->
      BoardMap.(
        remove coord1 curr_board
        |> remove coord2
        |> add coord2 curr_piece_moved)
  | exception Not_found ->
      BoardMap.(remove coord1 curr_board |> add coord2 curr_piece_moved)

let go_one_direction
    (coord : Coord.t)
    (board : t)
    (valid : Coord.t option list ref)
    (direction : Coord.t -> Coord.t) =
  let curr_coord = ref coord in
  let keepGoing = ref true in
  while !keepGoing do
    valid :=
      begin
        begin
          match Some (direction !curr_coord) with
          | Some x -> (
              match get_piece x board with
              | None ->
                  curr_coord := x;
                  Some x
              | Some p -> (
                  keepGoing := false;
                  match get_piece coord board with
                  | None -> None
                  | Some p2 ->
                      if Piece.get_color p <> Piece.get_color p2 then
                        Some (direction !curr_coord)
                      else None))
          | None ->
              None
              (*never get in this case just stopping compiler from
                complaining*)
          | exception Coord.OutOfBounds ->
              keepGoing := false;
              None
        end
        :: !valid
      end
  done

let rec elim_color
    (lst : Coord.t option list)
    (color : string)
    (board : t) : Coord.t option list =
  match lst with
  | [] -> []
  | h :: t -> (
      match h with
      | None -> elim_color t color board
      | Some c -> (
          match get_piece c board with
          | None -> h :: elim_color t color board
          | Some p ->
              if Piece.get_color p = color then elim_color t color board
              else h :: elim_color t color board))

(*determines if the squares to the upper left and upper right of the
  pawn are valid*)
let find_pawn_diagonals
    (coord : Coord.t)
    (board : t)
    (color : string)
    (direction1 : Coord.t -> Coord.t)
    (direction2 : Coord.t -> Coord.t) =
  begin
    begin
      match get_piece (direction1 coord) board with
      | Some p ->
          if Piece.get_color p = color then [ None ]
          else [ Some (direction1 coord) ] (*square is occupied*)
      | None -> [ None ] (*no piece*)
      | exception Coord.OutOfBounds -> [ None ] (*invalid square*)
    end
    @
    match get_piece (direction2 coord) board with
    | Some p ->
        if Piece.get_color p = color then [ None ]
        else [ Some (direction2 coord) ]
    | None -> [ None ]
    | exception Coord.OutOfBounds -> [ None ]
  end

(* determines if the pieces directly above the pawn are valid (accounts
   for if the pawn has moved or not)*)
let find_pawn_aboves
    (coord : Coord.t)
    (board : t)
    (piece : Piece.t)
    (direction : Coord.t -> Coord.t) =
  if Piece.get_moved piece = false then
    (*branch where piece hasn't moved*)
    let next_coord = direction coord in
    if get_piece next_coord board = None then
      (*branch where square above is empty*)
      let next_next_coord = direction next_coord in
      if get_piece next_next_coord board = None then
        (*branch where two suqares above is empty*)
        [ Some next_next_coord ] @ [ Some next_coord ]
      else (*branch where two squares above is occupied*) [ None ]
    else (*branch where square above is occupied*) [ None ]
  else
    (*branch where piece has moved*)
    match get_piece (direction coord) board with
    | Some _ -> [ None ] (*branch where square above is occupied*)
    | None ->
        [ Some (direction coord) ]
        (*branch where square above is empty*)
    | exception Coord.OutOfBounds -> [ None ]
(*branch where no square above*)

let pawn_moves (coord : Coord.t) (board : t) (piece : Piece.t) =
  if Piece.get_color piece = "white" then
    find_pawn_aboves coord board piece Coord.north
    @ find_pawn_diagonals coord board "white" Coord.northwest
        Coord.northeast
  else
    find_pawn_aboves coord board piece Coord.south
    @ find_pawn_diagonals coord board "black" Coord.southwest
        Coord.southeast

let rook_moves (coord : Coord.t) (board : t) =
  let valid = ref [] in
  go_one_direction coord board valid Coord.north;
  go_one_direction coord board valid Coord.south;
  go_one_direction coord board valid Coord.east;
  go_one_direction coord board valid Coord.west;
  !valid

let bishop_moves (coord : Coord.t) (board : t) =
  let valid = ref [] in
  go_one_direction coord board valid Coord.northwest;
  go_one_direction coord board valid Coord.northeast;
  go_one_direction coord board valid Coord.southeast;
  go_one_direction coord board valid Coord.southwest;
  !valid

let queen_moves (coord : Coord.t) (board : t) =
  let valid = ref [] in
  go_one_direction coord board valid Coord.north;
  go_one_direction coord board valid Coord.south;
  go_one_direction coord board valid Coord.east;
  go_one_direction coord board valid Coord.west;
  go_one_direction coord board valid Coord.northwest;
  go_one_direction coord board valid Coord.northeast;
  go_one_direction coord board valid Coord.southeast;
  go_one_direction coord board valid Coord.southwest;
  !valid

let knight_moves (coord : Coord.t) (board : t) =
  [
    (* each possible L-move that are still valid*)
    begin
      try Some (coord |> Coord.north |> Coord.north |> Coord.east)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.north coord |> Coord.north |> Coord.west)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.south coord |> Coord.south |> Coord.east)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.south coord |> Coord.south |> Coord.west)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.east coord |> Coord.east |> Coord.north)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.east coord |> Coord.east |> Coord.south)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.west coord |> Coord.west |> Coord.north)
      with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.west coord |> Coord.west |> Coord.south)
      with Coord.OutOfBounds -> None
    end;
  ]

let king_moves (coord : Coord.t) (board : t) =
  [
    (*all the cardinal directions*)
    begin
      try Some (Coord.north coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.south coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.west coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.east coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.northeast coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.northwest coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.southwest coord) with Coord.OutOfBounds -> None
    end;
    begin
      try Some (Coord.southeast coord) with Coord.OutOfBounds -> None
    end;
  ]

(*[in_check moves king] checks to see if any of the potential moves in
  [moves] is the king's current position [king]*)
let in_check (moves : Coord.t list) (king : Coord.t) : bool =
  List.exists (fun coord -> Coord.compare coord king == 0) moves

(*[find_king board is_white] finds the king of color [is_white] on the
  current board [board]. *)
let find_king (board : t) (is_white : bool) : Coord.t =
  let king_pos = ref ('A', 1) in
  for row = 1 to 8 do
    for col = 1 to 8 do
      let curr_coord = (Char.chr (col + 64), row) in
      match BoardMap.find_opt curr_coord board with
      | None -> ()
      | Some { is_white = white; name }
        when white = is_white && name = King ->
          king_pos := curr_coord
      | _ -> ()
    done
  done;
  !king_pos

(*[moves coord board] finds the moves of the piece at [coord] given the
  current board [board]. THIS FUNCTION DOES NOT ACCOUNT FOR CHECK.*)
let moves (coord : Coord.t) (board : t) =
  let move_list =
    match BoardMap.find_opt coord board with
    | Some p -> begin
        match Piece.get_name_str p with
        | "pawn" -> pawn_moves coord board p
        | "rook" -> rook_moves coord board
        | "bishop" ->
            elim_color
              (bishop_moves coord board)
              (Piece.get_color p) board
        | "knight" ->
            elim_color
              (knight_moves coord board)
              (Piece.get_color p) board
        | "queen" -> queen_moves coord board
        | _ ->
            elim_color
              (king_moves coord board)
              (Piece.get_color p) board (*king branch*)
      end
    | None -> []
  in
  let rec moves_only_valid (coords : Coord.t option list) =
    match coords with
    | [] -> []
    | h :: t -> (
        match h with
        | Some i -> i :: moves_only_valid t
        | None -> moves_only_valid t)
  in
  moves_only_valid move_list

(*[find_all_moves board is_white] finds all the valid moves of the
  current board [board]. Only finds moves of pieces with color specified
  by [is_white]. [is_white] is true when the pieces are white and false
  otherwise.*)
let find_all_moves (board : t) (is_white : bool) : Coord.t list =
  let all_moves = ref [] in
  for row = 1 to 8 do
    for col = 1 to 8 do
      let curr_coord = (Char.chr (col + 64), row) in
      match BoardMap.find_opt curr_coord board with
      | None -> ()
      | Some { is_white = white } when white <> is_white -> ()
      | _ -> all_moves := moves curr_coord board @ !all_moves
    done
  done;
  !all_moves

let rec valid_moves (coord : Coord.t) (board : t) (white_turn : bool) =
  let move_list = moves coord board in
  let rec moves_only_valid (coords : Coord.t list) =
    match coords with
    | [] -> []
    | h :: t ->
        if
          in_check_after_move coord h (find_king board white_turn) board
        then moves_only_valid t
        else h :: moves_only_valid t
  in
  moves_only_valid move_list

(*[in_check_after_move start dest king board] returns true if the king
  at position [king] is still in check after moving a piece from [start]
  to [dest].*)
and in_check_after_move
    (start : Coord.t)
    (dest : Coord.t)
    (king : Coord.t)
    (board : t) : bool =
  let new_board = update_board start dest board in
  let piece = BoardMap.find start board in
  if piece.name = King then
    in_check (find_all_moves new_board (not piece.is_white)) dest
  else in_check (find_all_moves new_board (not piece.is_white)) king

let move_is_valid
    (start : Coord.t)
    (dest : Coord.t)
    (board : t)
    (white_turn : bool) =
  let valid_moves = valid_moves start board white_turn in
  List.exists (fun coord -> Coord.compare coord dest == 0) valid_moves

let find_all_valid_moves_long (board : t) (is_white : bool) :
    (Coord.t * Coord.t) list =
  let rec prepend_start
      (start_coord : Coord.t)
      (end_coords : Coord.t list) =
    match end_coords with
    | [] -> []
    | h :: t -> (start_coord, h) :: prepend_start start_coord t
  in
  let all_moves = ref [] in
  for row = 1 to 8 do
    for col = 1 to 8 do
      let curr_coord = (Char.chr (col + 64), row) in
      match BoardMap.find_opt curr_coord board with
      | None -> ()
      | Some { is_white = white } when white <> is_white -> ()
      | _ ->
          all_moves :=
            prepend_start curr_coord
              (valid_moves curr_coord board is_white)
            @ !all_moves
    done
  done;
  !all_moves

(*same as [find_all_moves] except accounts for check*)
let find_all_valid_moves (board : t) (is_white : bool) : Coord.t list =
  let all_moves = ref [] in
  for row = 1 to 8 do
    for col = 1 to 8 do
      let curr_coord = (Char.chr (col + 64), row) in
      match BoardMap.find_opt curr_coord board with
      | None -> ()
      | Some { is_white = white } when white <> is_white -> ()
      | _ ->
          all_moves :=
            valid_moves curr_coord board is_white @ !all_moves
    done
  done;
  !all_moves

let game_state (board : t) (white_turn : bool) : state =
  let move_list = find_all_valid_moves board white_turn in
  match move_list with
  | h :: t -> Playing
  | [] ->
      if
        in_check
          (find_all_moves board (not white_turn))
          (find_king board white_turn)
      then Checkmate
      else Stalemate

let is_promotable (coord : Coord.t) (board : t) : bool =
  if
    (Coord.get_row coord = 8 || Coord.get_row coord = 1)
    && Piece.get_name_str (BoardMap.find coord board) = "pawn"
  then true
  else false

let promote (coord : Coord.t) (board : t) (piece : Piece.name) : t =
  let old_piece = BoardMap.find coord board in
  match piece with
  | Queen ->
      BoardMap.add coord
        { old_piece with name = piece; value = 9 }
        board
  | Knight ->
      BoardMap.add coord
        { old_piece with name = piece; value = 3 }
        board
  | Rook ->
      BoardMap.add coord
        { old_piece with name = piece; value = 5 }
        board
  | Bishop ->
      BoardMap.add coord
        { old_piece with name = piece; value = 3 }
        board
  | _ -> failwith "Cannot promote to this piece"
(*the last branch should never occur*)

(*Side notes. Also, include code for updating move history in game
  module.*)
