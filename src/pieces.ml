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

let get_position p = p.position

let is_king p = p.piece_type = King

let get_no_first_move p = p.no_first_move

let get_color p =
  match p.color with
  | White -> "W"
  | Black -> "B"
  | None -> "N"

let piece_picture (p : piece) =
  match (p.piece_type, p.color) with
  | Pawn, White -> "♙"
  | Pawn, Black -> "♟︎"
  | Bishop, White -> "♗"
  | Bishop, Black -> "♝"
  | Knight, White -> "♘"
  | Knight, Black -> "♞"
  | Rook, White -> "♖"
  | Rook, Black -> "♜"
  | Queen, White -> "♕"
  | Queen, Black -> "♛"
  | King, White -> "♔"
  | King, Black -> "♚"
  | _ -> " "

exception Illegal of string

let is_piece (p : piece) = p.piece_type != None

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

let pawn_legal_move (p : piece) (p2 : piece) =
  match (p.position, p2.position, p2.color) with
  | (initRow, initCol), (newRow, newCol), None ->
      initCol = newCol
      && (newRow - initRow = -1
         || (newRow - initRow = -2 && p.no_first_move))
  | (initRow, initCol), (newRow, newCol), _ ->
      newRow - initRow = -1
      && (newCol - initCol = -1 || newCol - initCol = 1)

let rook_is_legal ori_loc new_loc =
  (fst new_loc <> fst ori_loc && snd new_loc = snd ori_loc)
  || (snd new_loc <> snd ori_loc && fst new_loc = fst ori_loc)

let knight_is_legal ori_loc new_loc =
  abs (fst new_loc - fst ori_loc) = 2
  && abs (snd new_loc - snd ori_loc) = 1
  || abs (snd new_loc - snd ori_loc) = 2
     && abs (fst new_loc - fst ori_loc) = 1

let bishop_is_legal ori_loc new_loc =
  abs (fst new_loc - fst ori_loc) = abs (snd new_loc - snd ori_loc)

let queen_is_legal ori_loc new_loc =
  rook_is_legal ori_loc new_loc || bishop_is_legal ori_loc new_loc

let king_is_legal ori_loc new_loc =
  abs (fst ori_loc - fst new_loc) <= 1
  && abs (snd ori_loc - snd new_loc) <= 1

let id x = x

let is_legal (p : piece) (p2 : piece) =
  if p.color = p2.color then false
  else
    (* let piece = (what_piece st ori_loc) |> id in match
       piece.piece_type with *)
    match p.piece_type with
    | Pawn -> pawn_legal_move p p2
    | Rook -> rook_is_legal p.position p2.position
    | Knight -> knight_is_legal p.position p2.position
    | Bishop -> bishop_is_legal p.position p2.position
    | Queen -> queen_is_legal p.position p2.position
    | King -> king_is_legal p.position p2.position
    | None -> raise (Illegal "The original location has no piece.")

let can_castle (p1 : piece) (p3 : piece) =
  if p3.piece_type = Rook && p1.no_first_move && p3.no_first_move then
    true
  else false

let first_move (p : piece) = { p with no_first_move = false }

let new_loc_piece (p : piece) (new_loc : int * int) =
  { p with position = new_loc }

let piece_check_checker (p : piece) (p2 : piece) (p3 : piece) =
  if p = p2 then p3 else p2

let diag_check_piece (p : piece) (p2 : piece) (p3 : piece) =
  let pp = piece_check_checker p p2 p3 in
  if p.color = pp.color then false
  else if
    pp.piece_type = Queen || pp.piece_type = Bishop
    || (pp.piece_type = Pawn && fst pp.position - fst p.position = 1)
  then true
  else false

let orthog_adj_check_piece (p : piece) (p2 : piece) (p3 : piece) =
  let pp = piece_check_checker p p2 p3 in
  if p.color = pp.color then false
  else if pp.piece_type = Rook || pp.piece_type = Queen then true
  else false

let knight_check_piece (p : piece) (p2 : piece) =
  if p.color != p2.color && p2.piece_type = Knight then true else false
