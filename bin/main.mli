(** Pulls together other module to create playable chess game. *)

type location = int * int

type updateinfo =
  | Location of location
  | Castle of bool
  | Board of Chess.Board.t
  | State of Chess.State.t

val main : unit -> unit

val play_game :
  Chess.Board.t -> Chess.State.t -> string -> Chess.Board.t list -> unit
(** [keep_playing] is y or no depending on if player wants to play. If
    player doesn't input y or n, this function keeps asking for y or n. *)

val get_input_dest :
  Chess.Board.t -> Chess.State.t -> string -> updateinfo list
(** [get_input_dest brd st] contains the location of the selected piece,
    the dest location, and a bool for whether or not the move between
    these locations is castling.*)

val select_piece : Chess.State.t -> string -> location
(** [select_piece st] prompts player to select piece and calls helper to
    enable piece selection. Helepr to [get_input_dest]. *)

val get_sel_pce_loc : Chess.State.t -> string -> location
(**[get_sel_pce_loc state] is the location of the piece that the player
   wants to select. If player selects a location without a piece or
   invalid location, this function keeps asking for a board location
   until the user inputs a valid location with a piece on it. Helper to
   select_piece. *)

val get_dest :
  Chess.Board.t ->
  Chess.State.t ->
  location ->
  string ->
  updateinfo list
(**[get_dest brd state loc] prompts for destination location and calls
   [get_dest_loc]. *)

val get_dest_loc :
  Chess.Board.t ->
  Chess.State.t ->
  location ->
  string ->
  updateinfo list
(** [get_dest_loc brd st sel_loc] gets user input for the dest location.
    If user inputs “reselect,” then call [select_piece] to reselect the
    piece to move. If not, then call [chk_castl_and_legl] to check if
    the move to inputted dest location is legal. *)

val chk_castl_and_legl :
  Chess.Board.t ->
  Chess.State.t ->
  location ->
  string ->
  string ->
  updateinfo list
(** [chk_castl_and_legl brd st sel_loc dest_inpt] determines if input
    [dest_inpt] is a valid location and if the selected piece can
    legally move to that location. Also checks for if the move would be
    valid castling. If a valid move, calls [check_in_check]*)

val get_pce_on_dest : Chess.State.t -> location -> Chess.Pieces.piece
(** [get_pce_ont_dest st dest] is the piece on the location [dest]. If
    [dest] is not a valid location, then ask user for another location
    until valid location is inputted. *)

val get_rk_fin_loc : location -> location
(** [get_rk_fin_loc king_loc] returns the final location of the rook
    after castling. This depends on the destination position of the
    king. *)

val update_st_castle :
  Chess.State.t -> location -> location -> Chess.State.t
(**[update_st_castle st king_loc king_dest] is the state of the board
   after a castling move. *)

val update_st_norm_move :
  Chess.State.t -> location -> location -> Chess.State.t
(**[update_st_norm_move st sel_pce_loc dest] is the state of the board
   after a normal (non-castle) move. *)

val complete_castle :
  Chess.Board.t ->
  Chess.State.t ->
  location ->
  location ->
  updateinfo list
(** [complete_castle] is a list that has the updated board and state
    after a castling move. *)

val normal_move :
  Chess.Board.t ->
  Chess.State.t ->
  location ->
  location ->
  updateinfo list
(**[normal_move] is a list that includes the updated board and state
   after a normal (non-castle) move. *)

val keep_playing : unit -> string
(** [keep_playing] is y or no depending on if player wants to keep
    playing. If player doesn't input y or n, this function keeps asking
    for y or n. *)

val str_to_grid : string -> location
(**[str_to_grid st] converts a string input to a int * int tuple.
   Requries: [st is in format "(a,b)" where a and b are both ints]*)

val grid_to_str : location -> string
(**[grid_to_str loc] converts a int * int tuple to a string. *)

(* val get_updateinfo : updateinfo -> bool *)
(* This function may be needed to reduce code duplication. *)

(* ================================================================== *)
(* ================EXPLANATION OF HOW MAIN WORKS===================== *)
(* ================================================================== *)

(* Please ask if you're not clear on something. *)

(******************************************************************
  GENERAL OVERVIEW OF WHAT MAIN IS DOING

  To start game, call [main]. This calls [play_game], which loops
  (recursion actually) so the game continues.

  [play_game] calls [get_input_dest], which returns a list of the
  location of the selected piece and the destination location, and
  stores this list in a variable called [input_dest]. These locations
  are guaranteed to be valid, as [get_input_dest] calls numerous helpers
  to ensure that movement between the locations of the selected piece is
  legal and possible. [get_input_dest] and its helpers make up the bulk
  of main.ml. Details on this process to select a piece and dest
  location are given below in a separate labeled section. The list from
  [get_input_dest] also includes a boolean value that if true indicates
  that the move to be completed is a castling move.

  The values in the list [input_dest] are of variant type [updateinfo]
  because the items in the list are of different types (int*int,
  boolean,Board.t, State.t).

  After getting [input_dest], the information inside it (locations, bool
  for castling) is extracted and stored in separate variables.

  Next, to complete the actual move, either [complete_castle] or
  [normal_move] is called depending on whether or not the move is
  castling or not. Both of these functions returns a list that contains
  an updated board and state (type updateinfo list again). This board
  and state is extracted from the list and stored in variables in a
  similar way to how location/castling boolean was extracted from
  input_dest.

  The updated board is printed. The board is flipped (state also updated
  accordingly), and the player is prompted on whether to continue the
  game or not.

  *******************************************************************)

(** DETAIL ON GET_INPUT_DEST

    [get_input_dest] IS COMPRISED OF 2 PARTS:

    - (* -----FRST PART----------- *)

    - 1) SELECTING A PIECE -- CHOOSING A LOCATION (sel_pce_loc) This
      occurs through 2 helpers:

    - [select_piece]:
    - Prompts user for location and calls get_sel_pce_loc.

    - [get_sel_pce_loc]:
    - Obtains input and returns loc only if valid location selected.

    - (* -----SECOND PART--------- *)

    - 2) CHOOSING THE LOCATION TO MOVE THIS PIECE TO (DEST) AND
      RETURNING AN updateinfo list:

    - [get_dest]:
    - Initial prompt for destination location and call get_des_loc.

    - [get_dest_loc]:
    - Gets user input. If this is “reselect” then call select_piece to
      reselect the piece to move. If not, then call check_if_can move,
      passing in the current board, state, selected piece, and inputted
      location.

    - [chk_castl_and_legl]:
    - Determines if the chosen destination location is valid and then
      determines if the move proposed is valid (checks for castling at
      this point but does NOT check for whether king would be in check).
      If the move would be valid, then check_in_check is called.

    - [check_in_check]:
    - This function determines whether or not the proposed move would
      result in the current player’s king being in check. If in check,
      then this function prints error message and calls get_dest_loc to
      get new location. If not in check, then the move is a valid move,
      and an updateinfo list
      [Location sel_pce_loc; Location dest; Castle castle] is returned. *)
