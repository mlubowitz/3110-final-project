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
  en_passant : bool;
}

let get_piece_type (p : piece) =
  match p.piece_type with
  | Pawn -> "P"
  | Bishop -> "B"
  | Knight -> "N"
  | Rook -> "R"
  | Queen -> "Q"
  | King -> "K"
  | None -> "None"

let get_position p = p.position

let get_no_first_move p = p.no_first_move

let get_color p =
  match p.color with
  | White -> "W"
  | Black -> "B"
  | None -> "N"

let get_en_passant p = p.en_passant

exception Illegal of string

(* let piece_picture (p : piece) = match (p.piece_type, p.color) with |
   Pawn, White -> "♙" | Pawn, Black -> "♟︎" | Bishop, White -> "♗" |
   Bishop, Black -> "♝" | Knight, White -> "♘" | Knight, Black -> "♞" |
   Rook, White -> "♖" | Rook, Black -> "♜" | Queen, White -> "♕" |
   Queen, Black -> "♛" | King, White -> "♔" | King, Black -> "♚" | _ ->
   " " *)

let is_piece (p : piece) = p.piece_type != None

let to_piece (ori_loc : int * int) (n : string) : piece =
  match n with
  | "[P]" ->
      {
        piece_type = Pawn;
        color = Black;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[R]" ->
      {
        piece_type = Rook;
        color = Black;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[B]" ->
      {
        piece_type = Bishop;
        color = Black;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[N]" ->
      {
        piece_type = Knight;
        color = Black;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[Q]" ->
      {
        piece_type = Queen;
        color = Black;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[K]" ->
      {
        piece_type = King;
        color = Black;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[p]" ->
      {
        piece_type = Pawn;
        color = White;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[r]" ->
      {
        piece_type = Rook;
        color = White;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[b]" ->
      {
        piece_type = Bishop;
        color = White;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[n]" ->
      {
        piece_type = Knight;
        color = White;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[q]" ->
      {
        piece_type = Queen;
        color = White;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | "[k]" ->
      {
        piece_type = King;
        color = White;
        position = ori_loc;
        no_first_move = true;
        en_passant = false;
      }
  | _ ->
      {
        piece_type = None;
        color = None;
        position = ori_loc;
        no_first_move = false;
        en_passant = false;
      }

let pawn_legal_move (p : piece) (p2 : piece) =
  match (p.position, p2.position, p2.color) with
  | (initRow, initCol), (newRow, newCol), None ->
      initCol = newCol
      && (newRow - initRow = -1
         || (newRow - initRow = -2 && p.no_first_move))
  | (initRow, initCol), (newRow, newCol), _ ->
      newRow - initRow = -1 && abs (newCol - initCol) = 1

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

let is_legal_PIECES (p : piece) (p2 : piece) =
  let a, b = get_position p2 in
  if a < 0 || a > 7 || b < 0 || b > 7 then false
  else if p.color = p2.color then false
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

let en_passant_true (p : piece) = { p with en_passant = true }

let en_passant_false (p : piece) = { p with en_passant = false }

let new_loc_piece (p : piece) (new_loc : int * int) =
  { p with position = new_loc }

let diag_check_piece (p : piece) (p2 : piece) (p3 : piece) =
  let pp = if p = p2 then p3 else p2 in
  if p.color = pp.color then p
  else if
    pp.piece_type = Queen || pp.piece_type = Bishop
    || (pp.piece_type = Pawn && fst pp.position - fst p.position = 1)
  then pp
  else p

let orthog_adj_check_piece (p : piece) (p2 : piece) (p3 : piece) =
  let pp = if p = p2 then p3 else p2 in
  if p.color = pp.color then p
  else if pp.piece_type = Rook || pp.piece_type = Queen then pp
  else p

let knight_check_piece (p : piece) (p2 : piece) =
  if p.color != p2.color && p2.piece_type = Knight then p2 else p

let promotion_change (p : piece) (s : string) : piece =
  match s with
  | "P" -> { p with piece_type = Pawn }
  | "N" -> { p with piece_type = Knight }
  | "B" -> { p with piece_type = Bishop }
  | "R" -> { p with piece_type = Rook }
  | "Q" -> { p with piece_type = Queen }
  | _ -> failwith "not possible"