type piece =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Empty

exception Illegal of string

let to_piece = function
  | "[P]"
  | "[p]" ->
      Pawn
  | "[R]"
  | "[r]" ->
      Rook
  | "[N]"
  | "[n]" ->
      Knight
  | "[B]"
  | "[b]" ->
      Bishop
  | "[Q]"
  | "[q]" ->
      Queen
  | "[K]"
  | "[k]" ->
      King
  | _ -> Empty

let pawn_is_legal ori_loc new_loc = failwith "Unimplemented"

let rook_is_legal ori_loc new_loc =
  (fst new_loc <> fst ori_loc && snd new_loc = snd ori_loc)
  || (snd new_loc <> snd ori_loc && fst new_loc = fst ori_loc)

let knight_is_legal ori_loc new_loc = failwith "Unimplemented"

let bishop_is_legal ori_loc new_loc = failwith "Unimplemented"

let queen_is_legal ori_loc new_loc =
  rook_is_legal ori_loc new_loc || bishop_is_legal ori_loc new_loc

let king_is_legal ori_loc new_loc =
  abs (fst ori_loc - fst new_loc) <= 1
  && abs (snd ori_loc - snd new_loc) <= 1

let is_legal piece ori_loc new_loc =
  match to_piece piece with
  | Pawn -> pawn_is_legal ori_loc new_loc
  | Rook -> rook_is_legal ori_loc new_loc
  | Knight -> knight_is_legal ori_loc new_loc
  | Bishop -> bishop_is_legal ori_loc new_loc
  | Queen -> queen_is_legal ori_loc new_loc
  | King -> king_is_legal ori_loc new_loc
  | Empty -> raise (Illegal "The original location has no piece.")
