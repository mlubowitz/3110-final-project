(** Represents of pieces on the game board

    This module represents the game pieces and the movement of pieces 
    on the game board. *)

exception Illegal

type piece
(** The abstract type of values representing game pieces. *)

val piece_move : piece -> (*type representing game square*) -> (*new matrix*)
(** [piece_move p s] is the new matrix after piece [p] moves to square [s]. Raises
[Illegal] if the piece cannot move to square [s].*)

val is_legal : piece -> (*type representing game square*) -> bool
(** [is_legal p s] is the legality of moving piece [p] to square [s]. Raises
[Illegal] if the piece cannot move to square [s].*)