(** Represents of pieces on the game board

    This module represents the game pieces and the movement of pieces on
    the game board. *)

exception Illegal of string

type piece

val no_piece : piece -> bool
(** The abstract type of values representing game pieces. *)

val to_piece : int * int -> string -> piece
(** [to_piece s] is the conversion of a piece on the matrix to a piece
    type.*)

val is_legal : piece -> int * int -> bool
(** [is_legal piece ori_loc new_loc] is the legality of the movement of
    a given piece [piece] from [ori_loc] to [new_loc]. Raises
    [Illegal s] if there is no piece at [ori_loc]*)
