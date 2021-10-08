(* Representation of Chess board *)
(* Author: Chris Kim *)

type board
(**The abstract type representing an 8 x 8 chess board.  *)

type grid= int*int
(**The abstract type representing the location of a Chess grid.  *)

val init_board: unit -> board
(**[init_board] is the initial chess board with 16 pawns on either side of board.  *)

val piece_location: string -> grid
(** [piece_location p] is the location of piece [p] in the board. 
Requires: [p] is an actual piece on the board. *)

val move_piece: string -> grid -> board
(** [move_piece p n_loc] is the updated board with piece [p] moved to location [n_loc] *)

val flip: board -> board
(**[flip t] reverses the board [t] so that the top is now the bottom, and vice versa. *)