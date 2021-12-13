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

(*[update_st_once st loc pce] is a helper that does the work for
  [update_loc] - read the spec for [update_loc]. *)
let rec update_st_once st loc pce =
  (* iterate through st and keep adding the elements to a new list - if
     st location value matches loc, then insert pce at that location and
     skip over the list *)
  match st with
  | [] -> []
  | (l, p) :: t ->
      if l = loc then (l, pce) :: update_st_once t loc pce
      else (l, p) :: update_st_once t loc pce

let update_loc_helper (st : t) (loc : int * int) pce : t =
  let st_with_pce_moved = update_st_once st loc pce in
  let ori_loc = get_position pce in
  let fully_updated_st =
    update_st_once st_with_pce_moved ori_loc (to_piece ori_loc "[ ]")
  in
  fully_updated_st

(* =============st w/ only black queen; white king================== *)
let st_with_two_pces pce_loc =
  (* only leave white king and piece at loc l - for testing purposes *)
  List.map
    (fun x ->
      let loc = fst x in
      if loc = (7, 4) || loc = pce_loc then x
      else (loc, to_piece loc "[ ]"))
    (init_board () |> init_state)

(* ==================update_en_passant================================ *)
let update_en_passant (newloc : int * int) (p : piece) =
  if
    get_piece_type p = "P"
    && abs (fst (get_position p) - fst newloc) = 2
  then en_passant_true p
  else p

(* ==================en_passant_capture================================ *)
let is_en_passant (st : t) (p : piece) (p2 : piece) =
  match (get_position p, get_position p2, get_color p2) with
  | (initRow, initCol), (newRow, newCol), "N" ->
      newRow - initRow = -1
      && abs (newCol - initCol) = 1
      && get_en_passant (what_piece st (initRow, newCol))
  | (initRow, initCol), (newRow, newCol), _ -> false

(* ==================promotion======================================== *)
let promotion (st : t) (p : piece) (pdest : int * int) =
  get_piece_type p = "P" && fst pdest = 0 && fst (get_position p) = 1

let rec promotion_piece p =
  let () =
    print_endline "";
    print_endline
      "Pawn promotion: choose a replacement inputing as a string 'P' \n\
      \         for pawn, 'N' Knight, 'B' for bishop, 'R' for Rook, or \
       'Q' for Queen";
    print_string ">"
  in
  let input = read_line () in
  match input with
  | "P"
  | "N"
  | "B"
  | "R"
  | "Q" ->
      promotion_change p input
  | x ->
      let () =
        print_endline
          "Not a piece. Input location of piece you want to select.";
        print_string ">"
      in
      promotion_piece p

(* ==================update_board================================ *)
let update_board
    brd
    st
    sel_pce_loc
    dest
    (prmtion : bool)
    (enpsnt : bool) =
  if enpsnt then
    let capture =
      move_piece brd sel_pce_loc (fst sel_pce_loc, snd dest)
    in
    move_piece capture (fst sel_pce_loc, snd dest) dest
  else if prmtion then
    let piece = what_piece st dest in
    let piece_type = get_piece_type piece in
    let () = print_endline piece_type in
    if get_color piece = "W" then
      let piece_type = String.lowercase_ascii piece_type in
      promote_piece brd sel_pce_loc dest piece_type
    else promote_piece brd sel_pce_loc dest piece_type
  else move_piece brd sel_pce_loc dest

(* ==================update_loc================================ *)
let update_loc (st : t) (dest : int * int) (p : piece) =
  let p2 = what_piece st dest in
  if is_en_passant st p p2 then
    let st =
      update_loc_helper st
        (fst (get_position p), snd (get_position p2))
        p
    in
    update_loc_helper st (get_position p2) p
  else if promotion st p dest then
    update_loc_helper st dest (promotion_piece p)
  else update_loc_helper st dest p

(* ==================reset_en_passant================================ *)
let rec reset_helper color = function
  | l, p ->
      if color != get_color p then (l, en_passant_false p) else (l, p)

let reset_en_passant st color = List.map (reset_helper color) st

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
  if fst loc1 < fst loc2 - 1 then
    match is_piece (what_piece st (fst loc1 + 1, snd loc1)) with
    | false -> verticle_path_empty st (fst loc1 + 1, snd loc1) loc2
    | true -> what_piece st (fst loc1 + 1, snd loc1)
  else if fst loc1 > fst loc2 + 1 then
    match is_piece (what_piece st (fst loc1 - 1, snd loc1)) with
    | false -> verticle_path_empty st (fst loc1 - 1, snd loc1) loc2
    | true -> what_piece st (fst loc1 - 1, snd loc1)
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

let orthog_adj_check_help
    (st : t)
    (p : piece)
    (a : int)
    (b : int)
    (end_loc : int * int) =
  let a, b = get_position p in
  orthog_adj_check_piece p
    (piece_in_path st (a, b) (fst end_loc, snd end_loc))
    (what_piece st (fst end_loc, snd end_loc))

let in_check_orthog_adj (st : t) (p : piece) =
  let a, b = get_position p in
  if orthog_adj_check_help st p a b (a, 0) != p then
    orthog_adj_check_help st p a b (a, 0)
  else if orthog_adj_check_help st p a b (a, 7) != p then
    orthog_adj_check_help st p a b (a, 7)
  else if orthog_adj_check_help st p a b (0, b) != p then
    orthog_adj_check_help st p a b (0, b)
  else if orthog_adj_check_help st p a b (7, b) != p then
    orthog_adj_check_help st p a b (7, b)
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

(* ==================is_legal_castle======================================== *)
(*[is_legal_castle st p p2] returns if the king can castle moving from
  its location at p to the location of p2 *)
let castle_side (st : t) (p2 : piece) =
  if get_position p2 = (7, 6) then what_piece st (7, 7)
  else what_piece st (7, 0)

let is_legal_castle (st : t) (p : piece) (p2 : piece) =
  let pFst, pSnd = get_position p in
  let p2Fst, p2Snd = get_position p in
  let p3 = castle_side st p2 in
  get_piece_type p = "K"
  && in_check st p = false
  && abs (pSnd - p2Snd) = 2
  && pFst = p2Fst && can_castle p p3
  && is_path_empty st (get_position p) (get_position p3)
  &&
  if get_color p = "W" then
    in_check st (what_piece st (7, 4 + ((p2Snd - pSnd) / 2))) = false
  else in_check st (what_piece st (7, 3 + ((p2Snd - pSnd) / 2))) = false

(* ==================is_legal================================ *)
(*[is_legal st p p2] is [true] if given the state of the board [st],
  piece [p] can move from its current location to the location of [p2]*)
let is_legal (st : t) (p : piece) (p2 : piece) =
  if is_legal_PIECES p p2 then
    if get_piece_type p = "N" then true
    else if is_path_empty st (get_position p) (get_position p2) then
      if get_piece_type p != "K" then true else in_check st p2 = false
    else false
  else if get_piece_type p = "P" then is_en_passant st p p2
  else if get_piece_type p = "K" && is_legal_castle st p p2 then true
  else false

(* ==================possible_moves======================================== *)
let rec possible_moves t color = "unimplemented"

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

(* ==================state_to_list================================ *)

let state_to_list (st : t) : ((int * int) * piece) list = st

(* ==================insufficient_material================================ *)

let rec remaining_pieces_acc
    (state : t)
    (pieces : ((int * int) * piece) list)
    (acc : piece list) =
  match pieces with
  | [] -> acc
  | (l, p) :: t ->
      if get_piece_type p != "None" then
        remaining_pieces_acc state t (p :: acc)
      else remaining_pieces_acc state t acc

let remaining_pieces st =
  let t = state_to_list st in
  remaining_pieces_acc st t []

let prqExists (lst : piece list) =
  List.exists (fun x -> get_piece_type x = "P") lst
  || List.exists (fun x -> get_piece_type x = "Q") lst
  || List.exists (fun x -> get_piece_type x = "R") lst

let knight_endgame (lst : piece list) =
  List.filter (fun x -> get_piece_type x = "N" && get_color x = "W") lst
  |> List.length > 1
  || List.filter
       (fun x -> get_piece_type x = "N" && get_color x = "B")
       lst
     |> List.length > 1

let bishop_endgame (lst : piece list) =
  let white_bishops =
    List.filter
      (fun x -> get_piece_type x = "B" && get_color x = "W")
      lst
  in
  let black_bishops =
    List.filter
      (fun x -> get_piece_type x = "B" && get_color x = "W")
      lst
  in
  let white_loc_values =
    List.map
      (fun x -> (fst (get_position x) + snd (get_position x)) mod 2)
      white_bishops
  in
  let black_loc_values =
    List.map
      (fun x -> (fst (get_position x) + snd (get_position x)) mod 2)
      black_bishops
  in
  List.length white_bishops > 1
  && List.exists (fun x -> x = 0) white_loc_values
  && List.exists (fun x -> x = 1) white_loc_values
  || List.length black_bishops > 1
     && List.exists (fun x -> x = 0) black_loc_values
     && List.exists (fun x -> x = 1) black_loc_values

let insufficient_material st =
  let pieces = remaining_pieces st in
  if
    prqExists pieces = false
    && knight_endgame pieces = false
    && bishop_endgame pieces = false
  then true
  else false

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
  | 'a' -> (num - 1, 7)
  | 'b' -> (num - 1, 6)
  | 'c' -> (num - 1, 5)
  | 'd' -> (num - 1, 4)
  | 'e' -> (num - 1, 3)
  | 'f' -> (num - 1, 2)
  | 'g' -> (num - 1, 1)
  | 'h' -> (num - 1, 0)
  | _ -> failwith "not possible"

(*"alpha numeric" ==> (numeric - 1, alpha) *)
let alphanum_to_num (st : t) (input : string) color =
  let alphaNum = String.lowercase_ascii input in
  match color with
  | "W" -> white_alphanum_num alphaNum
  | "B" -> black_alphanum_num alphaNum
  | _ -> failwith "not possible"
