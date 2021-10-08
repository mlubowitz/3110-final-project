(** Represents the state of the chess game.

    The module represents the state of a chess game while it is being
    played, including what pieces have been captured, what pieces are 
    left on the board, and their positions.  *)


type t
(** Abstract type of values representing state of a chess game. state includes
which pieces have been captured for either side and which have not, and also
whose turn it is.*)

val capture: t -> t
(** [capture p] is the updated state with piece [p] captured.  *)

val location: t -> string
(**[location p] is the location of the piece [p] on the board.
IS THIS LOCATION A STRING OR AN INT?
*)

