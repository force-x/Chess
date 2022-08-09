exception OutOfBounds

type t = char * int

let from_string (s : string) =
  let string = String.uppercase_ascii s in
  let row = String.get string 0 in
  let col = int_of_string (String.make 1 (String.get string 1)) in
  (row, col)

let compare x y =
  let diff = int_of_char (fst x) - int_of_char (fst y) in
  if diff = 0 then snd x - snd y else diff

let string_of (coord : t) =
  String.lowercase_ascii (String.make 1 (fst coord))
  ^ string_of_int (snd coord)

let rec string_of_list (coords : t list) =
  match coords with
  | [] -> ""
  | h :: t -> string_of h ^ "\n" ^ string_of_list t

let get_col (coord : t) =
  match coord with
  | a, _ -> a

let get_row (coord : t) =
  match coord with
  | _, a -> a

let col_to_int (col : char) : int = Char.code col - 64
let int_to_col (i : int) : char = Char.chr (i + 64)

let get_color (coord : t) : string =
  match coord with
  | a, b ->
      let int_col = col_to_int a in
      if int_col mod 2 == b mod 2 then "dark" else "light"

(*Checks to make sure RI holds. That is col is between A-H and row is
  between 1-8*)
let rep_ok (col, row) =
  if row > 8 || row < 1 || col_to_int col < 1 || col_to_int col > 8 then
    raise OutOfBounds
  else (col, row)
(* (col, row) *)

let rep_ok_opt (col, row) =
  if row > 8 || row < 1 || col_to_int col < 1 || col_to_int col > 8 then
    None
  else Some (col, row)

(*Next column*)
let ( !++ ) x = x |> col_to_int |> ( + ) 1 |> int_to_col
let decr x = x - 1

(*Prev col*)
let ( !-- ) x = x |> col_to_int |> decr |> int_to_col
let north (col, row) = rep_ok (col, row + 1)
let south (col, row) = rep_ok (col, row - 1)
let west (col, row) = rep_ok (!--col, row)
let east (col, row) = rep_ok (!++col, row)
let northeast (col, row) = rep_ok (!++col, row + 1)
let northwest (col, row) = rep_ok (!--col, row + 1)
let southeast (col, row) = rep_ok (!++col, row - 1)
let southwest (col, row) = rep_ok (!--col, row - 1)
let north_opt (col, row) = rep_ok_opt (col, row + 1)
let south_opt (col, row) = rep_ok_opt (col, row - 1)
let west_opt (col, row) = rep_ok_opt (!--col, row)
let east_opt (col, row) = rep_ok_opt (!++col, row)
let northeast_opt (col, row) = rep_ok_opt (!++col, row + 1)
let northwest_opt (col, row) = rep_ok_opt (!--col, row + 1)
let southeast_opt (col, row) = rep_ok_opt (!++col, row - 1)
let southwest_opt (col, row) = rep_ok_opt (!--col, row - 1)
let north_unpr (col, row) = (col, row + 1)
let south_unpr (col, row) = (col, row - 1)
let west_unpr (col, row) = (!--col, row)
let east_unpr (col, row) = (!++col, row)
let northeast_unpr (col, row) = (!++col, row + 1)
let northwest_unpr (col, row) = (!--col, row + 1)
let southeast_unpr (col, row) = (!++col, row - 1)
let southwest_unpr (col, row) = (!--col, row - 1)
