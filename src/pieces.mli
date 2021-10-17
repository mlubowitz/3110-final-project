(** Represents of pieces on the game board

    This module represents the game pieces and the movement of pieces on
    the game board. *)

exception Illegal of string

type piece
(** The abstract type of values representing game pieces. *)

val to_piece : int * int -> string -> piece
(** [to_piece s] is the conversion of a piece on the matrix to a piece
    type.*)

val is_legal : string -> int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a given piece [piece] from [ori_loc] to [new_loc]. Raises
    [Illegal s] if there is no piece at [ori_loc]*)

val pawn_is_legal : int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a pawn from [ori_loc] to [new_loc].*)

val rook_is_legal : int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a rook from [ori_loc] to [new_loc].*)

val knight_is_legal : int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a knight from [ori_loc] to [new_loc].*)

val bishop_is_legal : int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a bishop from [ori_loc] to [new_loc].*)

val queen_is_legal : int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a queen from [ori_loc] to [new_loc].*)

val king_is_legal : int * int -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a king from [ori_loc] to [new_loc].*)
