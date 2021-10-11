type piece_type =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | None

type color =
  | White
  | Black
  | None

type position = int * int

type piece = {
  piece_type : piece_type;
  color : color;
  position : position;
  no_first_move : bool;
}

exception Illegal of string

let to_piece (ori_loc : int * int) (n : string) : piece =
  match n with
  | "[P]" ->
      {
        piece_type = Pawn;
        color = Black;
        position = ori_loc;
        no_first_move = true;
      }
  | "[R]" ->
      {
        piece_type = Rook;
        color = Black;
        position = ori_loc;
        no_first_move = true;
      }
  | "[B]" ->
      {
        piece_type = Bishop;
        color = Black;
        position = ori_loc;
        no_first_move = true;
      }
  | "[N]" ->
      {
        piece_type = Knight;
        color = Black;
        position = ori_loc;
        no_first_move = true;
      }
  | "[Q]" ->
      {
        piece_type = Queen;
        color = Black;
        position = ori_loc;
        no_first_move = true;
      }
  | "[K]" ->
      {
        piece_type = King;
        color = Black;
        position = ori_loc;
        no_first_move = true;
      }
  | "[p]" ->
      {
        piece_type = Pawn;
        color = White;
        position = ori_loc;
        no_first_move = true;
      }
  | "[r]" ->
      {
        piece_type = Rook;
        color = White;
        position = ori_loc;
        no_first_move = true;
      }
  | "[b]" ->
      {
        piece_type = Bishop;
        color = White;
        position = ori_loc;
        no_first_move = true;
      }
  | "[n]" ->
      {
        piece_type = Knight;
        color = White;
        position = ori_loc;
        no_first_move = true;
      }
  | "[q]" ->
      {
        piece_type = Queen;
        color = White;
        position = ori_loc;
        no_first_move = true;
      }
  | "[k]" ->
      {
        piece_type = King;
        color = White;
        position = ori_loc;
        no_first_move = true;
      }
  | _ ->
      {
        piece_type = None;
        color = None;
        position = ori_loc;
        no_first_move = false;
      }

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
