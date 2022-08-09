type command =
  | Help
  | Move of Coord.t * Coord.t
  | Moves of Coord.t
  | History
  | Show
  | Quit
  | Random

exception Empty
exception Malformed
exception BadSquare

let parse_square (sq : string) =
  if String.length sq != 2 then raise BadSquare
  else
    let l = String.get sq 0 in
    let n = String.get sq 1 in
    let l_upper = Char.uppercase_ascii l in
    let code_l = Char.code l_upper in
    let num = Char.code n - 48 in
    if code_l < 65 || code_l > 72 then raise BadSquare
    else
      let ch = Char.chr code_l in
      if num < 1 || num > 8 then raise BadSquare else (ch, num)

let parse str =
  if String.equal str "help" then Help
  else if String.equal str "quit" then Quit
  else if String.equal str "show" then Show
  else if String.equal str "history" then History
  else if String.equal str "random" then Random
  else
    let strs = String.split_on_char ' ' str in
    match List.length strs with
    | 0 -> raise Empty
    | 1 -> Moves (parse_square (List.hd strs))
    | 2 ->
        Move
          (parse_square (List.hd strs), parse_square (List.nth strs 1))
    | _ -> raise Malformed

let parse_promotion (str : string) : Piece.name =
  match str with
  | "knight" -> Knight
  | "rook" -> Rook
  | "bishop" -> Bishop
  | "queen" -> Queen
  | _ -> raise Malformed
