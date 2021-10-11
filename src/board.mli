(** Representation of Chess board

    This module represents the 8 x 8 chess board with chess pieces in
    it. This modules provides functions to update the board or flip the
    board and allows printing of the board to show the board in a clean
    and clear way.*)

(* Author: Chris Kim *)

type t
(**The abstract type representing an 8 x 8 chess board. *)

type grid = int * int
(**Represents the location of a Chess piece as a tuple (row,column).
   [(0,0)] is the top left grid on the board and [(7,7)] is the bottom
   right grid. *)

val init_board : unit -> t
(**[init_board ()] is the initial chess board with 16 pawns on either
   side of the board. *)

val move_piece : t -> grid -> grid -> t
(** [move_piece board ori_loc new_loc] is an updated board with the
    piece at [ori_loc] moved to location [new_loc]. While this function
    does return a new board, it also mutates the original [board] that
    is passed in as an argument, so it is possible to use the original
    [board] again in future code without assigning the output of
    [move_piece] to another identifer.*)

val flip : t -> t
(**[flip b] reverses the board [b] so that the top is now the bottom,
   and vice versa. It also reverses the contents of rows, so [flip b] is
   essentially the same as if you rotated the board 180 degrees. This
   function does not mutate the board passed in as an argument. Instead,
   it returns a completely new board.*)

val print_board : t -> unit
(** [print_board b] prints the board as a series of strings. *)
