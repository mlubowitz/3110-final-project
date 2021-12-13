(** Represents of pieces on the game board

    This module represents the game pieces and the movement of pieces on
    the game board. *)

exception Illegal of string

type piece
(** The abstract type of values representing game pieces. *)

val get_piece_type : piece -> string
(** [get_piece_type p] is the piece type of piece [p]*)

val get_position : piece -> int * int
(** [get_position p] is the position of piece [p]*)

val get_en_passant : piece -> bool
(** [get_en_passant p] is the en_passant value of piece [p]*)

val is_piece : piece -> bool
(** [is_piece p] is [true] if [p] is not [None] and [false] otherise*)

val to_piece : int * int -> string -> piece
(** [to_piece s] is the conversion of a piece on the matrix to a piece
    type.*)

val is_legal_PIECES : piece -> piece -> bool
(** [is_legal_PIECES piece piece] is the legality of the movement of a
    given piece [piece] from [ori_loc] to [new_loc]. Raises [Illegal s]
    if there is no piece at [ori_loc]. Exception: doesn't handle en
    passant*)

val first_move : piece -> piece
(** [first_move p] is the same piece as [p] but with no_first_move set
    to false.*)

val en_passant_true : piece -> piece
(** [en_passant_true p] is the same piece as [p] but with en_passant set
    to true.*)

val en_passant_false : piece -> piece
(** [en_passant_false p] is the same piece as [p] but with en_passant
    set to false.*)

val new_loc_piece : piece -> int * int -> piece
(** [new_loc_piece p new_loc] is the same piece as [p] but with position
    set to [new_loc].*)

val can_castle : piece -> piece -> bool
(** [can_castle p1 p3] returns [true] if [p3] is a rook and neither [p1]
    nor [p3] has moved, and [false] otherwise*)

val diag_check_piece : piece -> piece -> piece -> piece
(** [diag_check_piece p p2 p3] is [p] UNLESS [p2] or [p3] is
    bishop/queen's of opposite color. Return [p2] or [p3] if either is
    bishop/queen of opposite color. If both are bishop/queen of opposite
    color, return [p2].*)

(* To help understand above: [p2] is the piece that [piece_in_path] from
   state returns. Checks p2 or p3 color first, then it checks*)

val orthog_adj_check_piece : piece -> piece -> piece -> piece
(** [orthog_adj_check_piece p p2 p3] is [p] UNLESS [p2] or [p3] are
    rook/queen's of opposite color. Return [p2] or [p3] if either is
    rook/queen of opposite color. If both are rook/queen of opposite
    color, return [p2].*)

val knight_check_piece : piece -> piece -> piece
(** [knight_check_piece p p2] is [p2] if [p2] is a knight of opposite
    color to [p]. Otherwise, return [p].*)

val get_color : piece -> string
(** [get_color p] is either "W", "B", or "N" (no color) depending on the
    color of piece [p] . *)

val promotion_change : piece -> string -> piece
(**[promtion_change pce ]*)

val update_internal_loc : piece -> int * int -> piece
(* [update_internal_loc p loc] is piece [p] with its position field set
   to [loc]. *)
