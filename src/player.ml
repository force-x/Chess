type t = {
  pieces : Piece.t list;
  time : int;
}

let new_player = { pieces = []; time = 0 }

let add_to_pieces piece { pieces; time } =
  { pieces = piece :: pieces; time }

let get_captured (player: t) =
  player.pieces

let get_points (player : t) =
  let rec inner (pieces: Piece.t list) = 
    match pieces with
    | [] -> 0
    | h::t -> h.value + inner t
  in inner (get_captured player)

(*TIME NOT IMPLEMENTED YET*)
