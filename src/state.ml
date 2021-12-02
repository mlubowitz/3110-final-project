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
  else if fst loc1 < fst loc2 - 1 then
    match is_piece (what_piece st (fst loc1 + 1, snd loc1)) with
    | false -> verticle_path_empty st (fst loc1 + 1, snd loc1) loc2
    | true -> what_piece st (fst loc1 + 1, snd loc1)
  else what_piece st loc2

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
  else what_piece st loc2

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
  else what_piece st loc2

let piece_in_path (st : t) (loc1 : int * int) (loc2 : int * int) =
  if verticle_move loc1 loc2 then verticle_path_empty st loc1 loc2
  else if horizontal_move loc1 loc2 then
    horizontal_path_empty st loc1 loc2
  else if diagonal_move loc1 loc2 then diagonal_path_empty st loc1 loc2
  else what_piece st loc2

let is_path_empty (st : t) (loc1 : int * int) (loc2 : int * int) =
  get_position (piece_in_path st loc1 loc2) = loc2

(* ==================flip_state======================================== *)
(*[flip_loc] changes the location of the piece to reflect flipped
  location *)
let flip_loc = function
  | (r, c), p ->
      let new_l = (7 - r, 7 - c) in
      (new_l, new_loc_piece p new_l)

let flip_state st = List.map flip_loc st

(* ==================castle_side======================================== *)
(*[castle_side] returns the piece on the side that piece wantst to
  castle where the rook would be at the start of the game *)
let castle_side (st : t) (p2 : piece) =
  if get_position p2 = (7, 6) then what_piece st (7, 7)
  else what_piece st (7, 0)

(* ==================in_check======================================== *)
(*[in_check] determines if a player is in check *)

let diagonal_check_helper (st : t) (p : piece) =
  let a, b = get_position p in
  if a + b <= 7 then
    if
      diag_check_piece p
        (piece_in_path st (a, b) (0, a + b))
        (what_piece st (0, a + b))
      != p
    then
      diag_check_piece p
        (piece_in_path st (a, b) (0, a + b))
        (what_piece st (0, a + b)) (*top right diagonal*)
    else if
      diag_check_piece p
        (piece_in_path st (a, b) (a + b, 0))
        (what_piece st (a + b, 0))
      != p
    then
      diag_check_piece p
        (piece_in_path st (a, b) (a + b, 0))
        (what_piece st (a + b, 0))
    else p (*bottom left diagonal*)
  else if
    diag_check_piece p
      (piece_in_path st (a, b) (7 - (a - b), 7))
      (what_piece st (7 - (a - b), 7))
    != p
  then
    diag_check_piece p
      (piece_in_path st (a, b) (7 - (a - b), 7))
      (what_piece st (7 - (a - b), 7)) (*top right diagonal*)
  else if
    diag_check_piece p
      (piece_in_path st (a, b) (7, a + b - 7))
      (what_piece st (7, a + b - 7))
    != p
  then
    diag_check_piece p
      (piece_in_path st (a, b) (7, a + b - 7))
      (what_piece st (7, a + b - 7))
  else p

(*bottom left diagonal*)
let in_check_diagonals (st : t) (p : piece) : piece =
  let a, b = get_position p in
  (*row > column to check diagonals for top left and bottom right*)
  if a >= b then
    if
      diag_check_piece p
        (piece_in_path st (a, b) (a - b, 0))
        (what_piece st (a - b, 0))
      != p
    then
      diag_check_piece p
        (piece_in_path st (a, b) (a - b, 0))
        (what_piece st (a - b, 0)) (*top left diagonal*)
    else if
      diag_check_piece p
        (piece_in_path st (a, b) (7, 7 - (a - b)))
        (what_piece st (7, 7 - (a - b)))
      != p
    then
      diag_check_piece p
        (piece_in_path st (a, b) (7, 7 - (a - b)))
        (what_piece st (7, 7 - (a - b))) (*bottom right diagonal*)
    else if diagonal_check_helper st p != p then
      diagonal_check_helper st p
    else p
  else if
    diag_check_piece p
      (piece_in_path st (a, b) (0, b - a))
      (what_piece st (0, b - a))
    != p
  then
    diag_check_piece p
      (piece_in_path st (a, b) (0, b - a))
      (what_piece st (0, b - a)) (*top left diagonal*)
  else if
    diag_check_piece p
      (piece_in_path st (a, b) (7 - (b - a), 7))
      (what_piece st (7 - (b - a), 7))
    != p
  then
    diag_check_piece p
      (piece_in_path st (a, b) (7 - (b - a), 7))
      (what_piece st (7 - (b - a), 7)) (*bottom right diagonal*)
  else if diagonal_check_helper st p != p then
    diagonal_check_helper st p
  else p

let in_check_orthog_adj (st : t) (p : piece) =
  let a, b = get_position p in
  if
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (a, 0))
      (what_piece st (a, 0))
    != p
  then
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (a, 0))
      (what_piece st (a, 0))
  else if
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (a, 7))
      (what_piece st (a, 7))
    != p
  then
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (a, 7))
      (what_piece st (a, 7))
  else if
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (0, b))
      (what_piece st (0, b))
    != p
  then
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (0, b))
      (what_piece st (0, b))
  else if
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (7, b))
      (what_piece st (7, b))
    != p
  then
    orthog_adj_check_piece p
      (piece_in_path st (a, b) (7, b))
      (what_piece st (7, b))
  else p

let in_check_knight (st : t) (p : piece) =
  let a, b = get_position p in
  if
    a > 1 && b > 0
    && knight_check_piece p (what_piece st (a - 2, b - 1)) != p
  then knight_check_piece p (what_piece st (a - 2, b - 1))
  else if
    a > 1 && b < 7
    && knight_check_piece p (what_piece st (a - 2, b + 1)) != p
  then knight_check_piece p (what_piece st (a - 2, b + 1))
  else if
    a > 0 && b > 1
    && knight_check_piece p (what_piece st (a - 1, b - 2)) != p
  then knight_check_piece p (what_piece st (a - 1, b - 2))
  else if
    a > 0 && b < 6
    && knight_check_piece p (what_piece st (a - 1, b + 2)) != p
  then knight_check_piece p (what_piece st (a - 1, b + 2))
  else if
    a < 7 && b > 1
    && knight_check_piece p (what_piece st (a + 1, b - 2)) != p
  then knight_check_piece p (what_piece st (a + 1, b - 2))
  else if
    a < 7 && b < 6
    && knight_check_piece p (what_piece st (a + 1, b + 2)) != p
  then knight_check_piece p (what_piece st (a + 1, b + 2))
  else if
    a < 6 && b > 0
    && knight_check_piece p (what_piece st (a + 2, b - 1)) != p
  then knight_check_piece p (what_piece st (a + 2, b - 1))
  else if
    a < 6 && b < 7
    && knight_check_piece p (what_piece st (a + 2, b + 1)) != p
  then knight_check_piece p (what_piece st (a + 2, b + 1))
  else p

let in_check_piece (st : t) (p : piece) =
  let diagP = in_check_diagonals st p in
  let orthoAdjP = in_check_orthog_adj st p in
  let knightP = in_check_knight st p in
  if diagP != p then diagP
  else if orthoAdjP != p then orthoAdjP
  else if knightP != p then knightP
  else p

let in_check (st : t) (p : piece) = in_check_piece st p != p

(* ==================find_king======================================== *)
let rec find_king t color =
  match t with
  | [] -> failwith "piece should be in board."
  | (l, p) :: t ->
      if get_piece_type p = "K" && color = get_color p then l
      else find_king t color

(* ==================promotion======================================== *)
let promotion (st : t) (p : piece) (pdest : int * int) =
  get_piece_type p = "P" && fst pdest = 0

(* ==================possible_moves======================================== *)
let rec possible_moves t color = "unimplemented"

(* ==================stalemate============================================= *)

let move_up1_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst - 1, pSnd) in
  is_legal p1 p2 && in_check st p2 = false

let move_down1_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst + 1, pSnd) in
  is_legal p1 p2 && in_check st p2 = false

let move_left1_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst, pSnd - 1) in
  is_legal p1 p2 && in_check st p2 = false

let move_right1_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst, pSnd + 1) in
  is_legal p1 p2 && in_check st p2 = false

let move_URdiag_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst - 1, pSnd + 1) in
  is_legal p1 p2 && in_check st p2 = false

let move_LRdiag_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst + 1, pSnd + 1) in
  is_legal p1 p2 && in_check st p2 = false

let move_ULdiag_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst - 1, pSnd - 1) in
  is_legal p1 p2 && in_check st p2 = false

let move_LLdiag_helper (st : t) (p1 : piece) (pFst : int) (pSnd : int) =
  let p2 = what_piece st (pFst + 1, pSnd - 1) in
  is_legal p1 p2 && in_check st p2 = false

let can_not_knight_move (st : t) (p1 : piece) =
  let pFst, pSnd = get_position p1 in
  move_up1_helper st p1 pFst pSnd
  || move_down1_helper st p1 pFst pSnd
  || move_left1_helper st p1 pFst pSnd
  || move_right1_helper st p1 pFst pSnd
  || move_URdiag_helper st p1 pFst pSnd
  || move_LRdiag_helper st p1 pFst pSnd
  || move_ULdiag_helper st p1 pFst pSnd
  || move_LLdiag_helper st p1 pFst pSnd

let can_knight_move (st : t) (knight : piece) =
  let a, b = get_position knight in
  is_legal knight (what_piece st (a - 2, b - 1))
  || is_legal knight (what_piece st (a - 2, b + 1))
  || is_legal knight (what_piece st (a - 1, b - 2))
  || is_legal knight (what_piece st (a - 1, b + 2))
  || is_legal knight (what_piece st (a + 1, b - 2))
  || is_legal knight (what_piece st (a + 1, b + 2))
  || is_legal knight (what_piece st (a + 2, b - 1))
  || is_legal knight (what_piece st (a + 2, b + 1))

let can_piece_move (st : t) (p : piece) =
  if get_piece_type p != "N" then can_not_knight_move st p
  else can_knight_move st p

let rec stalemate t color =
  match t with
  | [] -> true
  | (l, p) :: t ->
      if color = get_color p && can_piece_move t p then false
      else stalemate t color

(* ==================checkmate======================================== *)

let is_inbetween l p1 p2 c1 c2 =
  fst l < max p1 c1
  && fst l > min p1 c1
  && snd l < max p2 c2
  && snd l > min p2 c2

let rec path_list_acc t acc p1 p2 c1 c2 =
  match t with
  | [] -> acc
  | (l, p) :: t ->
      if is_inbetween l p1 p2 c1 c2 = false then
        path_list_acc t acc p1 p2 c1 c2
      else if diagonal_move (p1, p2) (c1, c2) then
        if diagonal_move (p1, p2) l then
          path_list_acc t (l :: acc) p1 p2 c1 c2
        else path_list_acc t acc p1 p2 c1 c2
      else if horizontal_move (p1, p2) (c1, c2) then
        if horizontal_move (p1, p2) l then
          path_list_acc t (l :: acc) p1 p2 c1 c2
        else path_list_acc t acc p1 p2 c1 c2
      else if verticle_move (p1, p2) (c1, c2) then
        if verticle_move (p1, p2) l then
          path_list_acc t (l :: acc) p1 p2 c1 c2
        else path_list_acc t acc p1 p2 c1 c2
      else path_list_acc t acc p1 p2 c1 c2

let path_list t c1 c2 p1 p2 = path_list_acc t [ (c1, c2) ] p1 p2 c1 c2

let checkpath_list (st : t) (p : piece) =
  let checkPiece = in_check_piece st p in
  let c1, c2 = get_position checkPiece in
  let p1, p2 = get_position p in
  if get_piece_type checkPiece = "N" then []
  else path_list st p1 p2 c1 c2

let state_to_list (st : t) : ((int * int) * piece) list = st

(* ==================alphanum_to_num================================ *)

let white_alphanum_num (input : string) =
  let num = input.[1] |> Char.escaped |> int_of_string in
  match input.[0] with
  | 'a' -> (8 - num, 0)
  | 'b' -> (8 - num, 1)
  | 'c' -> (8 - num, 2)
  | 'd' -> (8 - num, 3)
  | 'e' -> (8 - num, 4)
  | 'f' -> (8 - num, 5)
  | 'g' -> (8 - num, 6)
  | 'h' -> (8 - num, 7)
  | _ -> failwith "not possible"

let black_alphanum_num (input : string) =
  let num = input.[1] |> Char.escaped |> int_of_string in
  match input.[0] with
  | 'a' -> (8 - num, 0)
  | 'b' -> (8 - num, 1)
  | 'c' -> (8 - num, 2)
  | 'd' -> (8 - num, 3)
  | 'e' -> (8 - num, 4)
  | 'f' -> (8 - num, 5)
  | 'g' -> (8 - num, 6)
  | 'h' -> (8 - num, 7)
  | _ -> failwith "not possible"

(*"alpha numeric" ==> (numeric - 1, alpha) *)
let alphanum_to_num (st : t) (input : string) color =
  let alphaNum = String.lowercase_ascii input in
  match color with
  | "W" -> white_alphanum_num alphaNum
  | "B" -> black_alphanum_num alphaNum
  | _ -> failwith "not possible"
