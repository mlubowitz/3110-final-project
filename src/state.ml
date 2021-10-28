open Pieces
open Board

type t = ((int * int) * piece) list

exception InvalidLocation of (int * int)

(* [r_to_dict board num_el row_num] is a dictionary that contains pairs
   of (location, piece info) of the the [row_num]th row in the matrix.
   The created dictionary has the leftmost pieces in the board at the
   start of the dictionary and the vice versa. [num_el] is originally
   the index of the first element in the list (0) and is the index of
   the elemnt currently being put into the dictionary. This is a helper
   to [init_state] and creating an association list for location and
   pieces.*)
let rec r_to_dict board num_el row_num : t =
  match num_el with
  | 8 -> []
  | x ->
      ( (row_num, x),
        to_piece (row_num, x) (get_str_piece board (row_num, x)) )
      :: r_to_dict board (num_el + 1) row_num

let init_state board : t =
  let rec make_dict row_ind =
    match row_ind with
    | 8 -> []
    | x -> r_to_dict board 0 x :: make_dict (row_ind + 1)
  in
  List.flatten (make_dict 0)

(* Exapmle: [ ((0,0), Pawn); ((0,1), Pawn) ((0,2), Pawn) ] *)

(* ==================what_piece======================================== *)

let rec what_piece (st : t) (loc : int * int) =
  match st with
  | [] -> raise (InvalidLocation loc)
  | (l, p) :: t -> if l = loc then p else what_piece t loc

(* ==================update_loc======================================== *)

let rec update_loc (st : t) (loc : int * int) pce : t =
  (* iterate through st and keep adding the elements to a new list - if
     st location value matches loc, then insert pce at that location and
     skip over the list *)
  match st with
  | [] -> []
  | (l, p) :: t ->
      if l = loc then (l, pce) :: update_loc t loc pce
      else (l, p) :: update_loc t loc pce

(* ==================is_path_empty======================================== *)
let verticle_move (loc1 : int * int) (loc2 : int * int) =
  snd loc1 = snd loc2 && fst loc1 != fst loc2

let horizontal_move (loc1 : int * int) (loc2 : int * int) =
  snd loc1 != snd loc2 && fst loc1 = fst loc2

let diagonal_move (loc1 : int * int) (loc2 : int * int) =
  abs (fst loc1 - fst loc2) = abs (snd loc1 - snd loc2)

(* [verticle_path_empty st loc1 loc2] is true if path from [loc1] to
   [loc2] has no pieces. Otherwise false. This is a hlper to
   [is_path_empty] *)
let rec verticle_path_empty
    (st : t)
    (loc1 : int * int)
    (loc2 : int * int) =
  if fst loc1 > fst loc2 + 1 then
    match is_piece (what_piece st (fst loc1 - 1, snd loc1)) with
    | false -> verticle_path_empty st (fst loc1 - 1, snd loc1) loc2
    | true -> what_piece st (fst loc1 - 1, snd loc1)
  else if fst loc2 > fst loc1 + 1 then
    match is_piece (what_piece st (fst loc2 - 1, snd loc2)) with
    | false -> verticle_path_empty st (fst loc2 - 1, snd loc2) loc1
    | true -> what_piece st (fst loc2 - 1, snd loc2)
  else what_piece st loc1

(* [horizontal_path_empty st loc1 loc2] is true if path from [loc1] to
   [loc2] has no pieces. Otherwise false. This is a hlper to
   [is_path_empty] *)
let rec horizontal_path_empty
    (st : t)
    (loc1 : int * int)
    (loc2 : int * int) =
  if snd loc1 < snd loc2 - 1 then
    match is_piece (what_piece st (fst loc1, snd loc1 + 1)) with
    | false -> horizontal_path_empty st (fst loc1, snd loc1 + 1) loc2
    | true -> what_piece st (fst loc1, snd loc1 + 1)
  else if snd loc1 > snd loc2 + 1 then
    match is_piece (what_piece st (fst loc1, snd loc1 - 1)) with
    | false -> horizontal_path_empty st (fst loc1, snd loc1 - 1) loc2
    | true -> what_piece st (fst loc1, snd loc1 - 1)
  else what_piece st loc1

(* [diagonal_path_empty st loc1 loc2] is true if path from [loc1] to
   [loc2] has no pieces. Otherwise false. This is a hlper to
   [is_path_empty] *)
let rec diagonal_path_empty
    (st : t)
    (loc1 : int * int)
    (loc2 : int * int) =
  if fst loc1 < fst loc2 - 1 && snd loc1 < snd loc2 - 1 then
    match is_piece (what_piece st (fst loc1 + 1, snd loc1 + 1)) with
    | false -> diagonal_path_empty st (fst loc1 + 1, snd loc1 + 1) loc2
    | true -> what_piece st (fst loc1 + 1, snd loc1 + 1)
  else if fst loc1 < fst loc2 - 1 && snd loc1 > snd loc2 + 1 then
    match is_piece (what_piece st (fst loc1 + 1, snd loc1 - 1)) with
    | false -> diagonal_path_empty st (fst loc1 + 1, snd loc1 - 1) loc2
    | true -> what_piece st (fst loc1 + 1, snd loc1 - 1)
  else if fst loc1 > fst loc2 + 1 && snd loc1 > snd loc2 + 1 then
    match is_piece (what_piece st (fst loc1 - 1, snd loc1 - 1)) with
    | false -> diagonal_path_empty st (fst loc1 - 1, snd loc1 - 1) loc2
    | true -> what_piece st (fst loc1 - 1, snd loc1 - 1)
  else if fst loc1 > fst loc2 + 1 && snd loc1 < snd loc2 - 1 then
    match is_piece (what_piece st (fst loc1 - 1, snd loc1 + 1)) with
    | false -> diagonal_path_empty st (fst loc1 - 1, snd loc1 + 1) loc2
    | true -> what_piece st (fst loc1 - 1, snd loc1 + 1)
  else what_piece st loc1

let piece_in_path (st : t) (loc1 : int * int) (loc2 : int * int) =
  if verticle_move loc1 loc2 then verticle_path_empty st loc1 loc2
  else if horizontal_move loc1 loc2 then
    horizontal_path_empty st loc1 loc2
  else if diagonal_move loc1 loc2 then diagonal_path_empty st loc1 loc2
  else what_piece st loc1

let is_path_empty (st : t) (loc1 : int * int) (loc2 : int * int) =
  if get_position (piece_in_path st loc1 loc2) = loc1 then true
  else false

(* ==================flip_state======================================== *)
(*[flip_loc] changes the location of the piece to reflect flipped
  location *)
let flip_loc = function
  | (r, c), p ->
      let new_l = (7 - r, 7 - c) in
      (new_l, new_loc_piece p new_l)

let flip_state st = List.map flip_loc st

(* ==================castle_side======================================== *)
(*[castle_side] returns the piece on the side that king wantst to castle
  where the rook would be at the start of the game *)
let castle_side (st : t) (p2 : piece) =
  if get_position p2 = (7, 6) then what_piece st (7, 7)
  else what_piece st (7, 0)

(* ==================in_check======================================== *)
(*[in_check] determines if a player is in check *)

let diagonal_check_helper (st : t) (p : piece) =
  let a, b = get_position p in
  if a + b <= 7 then
    diag_check_piece (piece_in_path st (a, b) (-1, a + b)) p
    (*top right diagonal*)
    && diag_check_piece (piece_in_path st (a, b) (a + b, -1)) p
    (*bottom left diagonal*)
  else
    diag_check_piece (piece_in_path st (a, b) (a - b, 8)) p
    (*top right diagonal*)
    && diag_check_piece (piece_in_path st (a, b) (8, a + b - 8)) p

(*bottom left diagonal*)
let in_check_diagonals (st : t) (p : piece) =
  let a, b = get_position p in
  (*row > column to check diagonals for top left and bottom right*)
  if a >= b then
    if
      diag_check_piece (piece_in_path st (a, b) (a - b, -1)) p
      (*top left diagonal*)
      && diag_check_piece (piece_in_path st (a, b) (8, 8 - (a - b))) p
      (*bottom right diagonal*)
    then diagonal_check_helper st p
    else false
  else if a < b then
    if
      diag_check_piece (piece_in_path st (a, b) (-1, b - a)) p
      (*top left diagonal*)
      && diag_check_piece (piece_in_path st (a, b) (8 - (b - a), 8)) p
      (*bottom right diagonal*)
    then diagonal_check_helper st p
    else false
  else false

let in_check_orthog_adj (st : t) (p : piece) =
  let a, b = get_position p in
  if a > 0 && b > 0 && a < 7 && b < 7 then
    orthog_adj_check_piece (piece_in_path st (a, b) (a, -1)) p
    && orthog_adj_check_piece (piece_in_path st (a, b) (a, 8)) p
    && orthog_adj_check_piece (piece_in_path st (a, b) (-1, b)) p
    && orthog_adj_check_piece (piece_in_path st (a, b) (8, b)) p
  else false

let in_check_knight (st : t) (p : piece) = failwith "Unimplemented"
