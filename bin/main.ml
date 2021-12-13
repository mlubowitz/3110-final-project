open Chess
open Board
open State
open Pieces

type location = int * int

type updateinfo =
  | Location of location
  | Castle of bool
  | Board of Board.t
  | State of State.t

(* ================================================================== *)
(* ====================START HELPER FUNCTIONS======================== *)
(* ================================================================== *)
(* ================================================================== *)

(*Converts str version of tuple to a regular tuple. Example: ["(3,5)"
  becomes (3,5)]. *)
let str_to_grid str =
  (int_of_char str.[1] - 48, int_of_char str.[3] - 48)

(* ======================================================= *)

(*Converts regular version of tuple to a string. Example: [(3,5) becomes
  "(3,5)"]. *)
let grid_to_str grid =
  "(" ^ string_of_int (fst grid) ^ "," ^ string_of_int (snd grid) ^ ")"

(* ======================================================= *)

let rec keep_playing () =
  let input = read_line () in
  match input with
  | "y"
  | "Y"
  | "yes"
  | "Yes" ->
      "y"
  | "n"
  | "N"
  | "no"
  | "No" ->
      "n"
  | x ->
      let () = print_endline "Please type y or n" in
      keep_playing ()

(* ================================================================== *)

let get_rk_fin_loc king_dest =
  match king_dest with
  | 7, 6 -> (7, 5)
  | 7, 5 -> (7, 4)
  | 7, 2 -> (7, 3)
  | 7, 1 -> (7, 2)
  | _ -> failwith "This should never be reached"

(* ================================================================== *)

let update_st_castle st king_loc king_dest =
  let rook_loc =
    get_position (castle_side st (what_piece st king_dest))
  in
  let rook_fin_loc = get_rk_fin_loc king_dest in
  let moved_king = what_piece st king_loc |> first_move in
  let moved_rook = what_piece st rook_loc |> first_move in
  let st = update_loc st king_dest moved_king in
  update_loc st rook_fin_loc moved_rook
(* let st = update_loc st king_loc (to_piece king_loc "[ ]") in let st =
   update_loc st rook_loc (to_piece rook_loc "[ ]") in *)

(* ========================================================= *)

let complete_castle brd st king_loc king_dest =
  let rook_fin_loc = get_rk_fin_loc king_dest in

  let brd = move_piece brd king_loc king_dest in
  let brd =
    match king_dest with
    | 7, 6
    | 7, 5 ->
        move_piece brd (7, 7) rook_fin_loc
    | 7, 2
    | 7, 1 ->
        move_piece brd (7, 0) rook_fin_loc
    | _ -> failwith "This should never be reached"
  in

  let new_st = update_st_castle st king_loc king_dest in

  [ Board brd; State new_st ]

(* ============================================================ *)

let update_st_norm_move st sel_pce_loc dest =
  (* Get the moved piece and mark it as having moved at least once. *)
  let moved_piece =
    what_piece st sel_pce_loc |> first_move |> update_en_passant dest
  in
  update_loc st dest moved_piece
(* let st = update_loc st sel_pce_loc (to_piece sel_pce_loc "[ ]") in *)

(* ======================================================= *)

let list_to_string (lst : (int * int) list) =
  List.fold_left
    (fun acc x ->
      acc ^ "("
      ^ string_of_int (fst x)
      ^ ", "
      ^ string_of_int (snd x)
      ^ ")   ")
    "" lst

let king_legal_move (st : t) (p : piece) (dest : int * int) =
  let newKing = new_loc_piece p dest in
  if in_check st newKing then false else true

let rec possible_moves_list_acc
    (state : State.t)
    (pieces : ((int * int) * piece) list)
    color
    (destinations : (int * int) list)
    (acc : (int * int) list) =
  match pieces with
  | [] -> acc
  | (l, p) :: t ->
      if get_color p = color then
        let unChecked = destinations in
        check_all_dests state l p t color destinations unChecked acc
      else possible_moves_list_acc state t color destinations acc

and check_all_dests state l p t color destinations unChecked acc =
  match unChecked with
  | [] -> possible_moves_list_acc state t color destinations acc
  | k :: m ->
      if is_legal state p (what_piece state k) then
        (* let king = find_king state color |> what_piece state in if p
           = king && is_legal_castle state p (what_piece state k) =
           false then if king_legal_move state p k then let () =
           print_endline "legal" in check_all_dests state l p t color
           destinations m (k :: acc) else let () = print_endline "not
           legal" in check_all_dests state l p t color destinations m
           acc else *)
        let st_w_move =
          if is_legal_castle state p (what_piece state k) then
            update_st_castle state l k
          else update_st_norm_move state l k
        in
        let kingloc =
          find_king st_w_move color
          |> what_piece st_w_move |> get_position
        in
        let () = print_endline (list_to_string [ kingloc ]) in
        let is_in_check =
          in_check st_w_move
            (find_king st_w_move color |> what_piece st_w_move)
        in
        if is_in_check then
          check_all_dests state l p t color destinations m acc
        else check_all_dests state l p t color destinations m (k :: acc)
      else check_all_dests state l p t color destinations m acc

(*[possible_moves_list st pieces color destinations] creates a list of
  all the locations on the board, [destinations], to which pieces on the
  board in list [pieces] of the given [color] can move*)
let possible_moves_list st pieces color destinations =
  possible_moves_list_acc st pieces color destinations []

(* If the king is in check, given the color and state this determines if
   that check is checkamte*)
let checkmate (st : t) color =
  let king = what_piece st (find_king st color) in
  if in_check st king then
    let t = state_to_list st in
    let locs = List.map (fun x -> fst x) t in
    possible_moves_list st t color locs = []
  else false

let stalemate (st : t) color =
  let king = what_piece st (find_king st color) in
  if in_check st king then false
  else
    let t = state_to_list st in
    let locs = List.map (fun x -> fst x) t in
    List.length (possible_moves_list st t color locs) = 0

let piece_possible_moves (st : t) (p : piece) =
  let t = state_to_list st in
  let locs = List.map (fun x -> fst x) t in
  let piece = (get_position p, p) in
  let color = get_color p in
  possible_moves_list st [ piece ] color locs

(* ======================================================= *)

let rec get_sel_pce_loc state player_turn =
  let input = read_line () |> str_to_grid in
  match what_piece state input with
  | exception InvalidLocation e ->
      let () =
        print_endline
          "Not valid board location. Input another location.";
        print_string ">"
      in
      get_sel_pce_loc state player_turn
  | p -> (
      let piece = p in
      match
        is_piece piece
        && get_color piece = player_turn
        && piece_possible_moves state piece != []
      with
      | true ->
          let () =
            print_endline
              (list_to_string (piece_possible_moves state piece))
          in
          input
      | false ->
          let () =
            print_endline
              "Not a valid piece, or this piece does not have any \
               valid moves, or wrong color. Input location of piece \
               you want to select.";
            print_string ">"
          in
          get_sel_pce_loc state player_turn)

(* ================================================================== *)

let select_piece st player_turn =
  let () =
    print_endline "";
    print_endline
      "Select piece you want to move by inputting its location in \
       EXACT format (row,column). Upper left is (0,0) and bottom right \
       is (7,7). NO SPACES!";
    print_string ">"
  in
  get_sel_pce_loc st player_turn

(* ============================================================ *)

let rec normal_move brd st sel_pce_loc dest =
  let p = what_piece st sel_pce_loc in
  let p2 = what_piece st dest in
  let prmtion = promotion st p dest in
  let enpsnt = is_en_passant st p p2 in
  let new_st = update_st_norm_move st sel_pce_loc dest in
  let new_brd =
    update_board brd new_st sel_pce_loc dest prmtion enpsnt
  in
  [ Board new_brd; State new_st ]

(* ======================================================= *)

let rec get_pce_on_dest state dest =
  match what_piece state dest with
  | exception InvalidLocation e ->
      let () =
        print_endline
          "Not valid board location. Input another location.";
        print_string ">"
      in
      let dest = read_line () |> str_to_grid in
      get_pce_on_dest state dest
  | p -> p

(* =================================================== *)

let rec get_dest_loc brd state sel_pce_loc player_turn =
  let dest = read_line () in
  match dest with
  | "reselect"
  | "'reselect'" ->
      let new_pce = select_piece state player_turn in
      let () =
        print_endline "";
        print_endline
          ("CURRENTLY SELECTED PIECE: "
          ^ get_str_piece brd new_pce
          ^ " at " ^ grid_to_str new_pce)
      in
      let () =
        print_endline
          "Input destination location in EXACT format (row,column). \
           Upper left is (0,0) and bottom right is (7,7). NO SPACES! \
           Or type 'reselect' to select a different piece.";
        print_string ">"
      in
      get_dest_loc brd state new_pce player_turn
  | x -> chk_castl_and_legl brd state sel_pce_loc x player_turn

(* =======MUTUAL RECURSION=================== *)

and chk_castl_and_legl brd state sel_pce_loc dest_inpt player_turn =
  let dest = dest_inpt |> str_to_grid in
  let selected_piece = what_piece state sel_pce_loc in
  let pce_on_dest = get_pce_on_dest state dest in

  (* dest may have changed in get_pce_on_dest *)
  let dest = get_position pce_on_dest in
  let possible_dests = piece_possible_moves state selected_piece in

  match List.exists (fun x -> x = dest) possible_dests with
  | false ->
      let () =
        print_endline
          "Your selected piece cannot move to that location. Input new \
           destination location or type 'reselect'.";
        print_string ">"
      in
      get_dest_loc brd state sel_pce_loc player_turn
  | true -> (
      match is_legal_castle state selected_piece pce_on_dest with
      | true -> [ Location sel_pce_loc; Location dest; Castle true ]
      | false -> [ Location sel_pce_loc; Location dest; Castle false ])
(* ============================================================ *)

let get_dest brd state sel_pce_loc player_turn =
  let () =
    print_endline "";
    print_endline
      ("CURRENTLY SELECTED PIECE: "
      ^ get_str_piece brd sel_pce_loc
      ^ " at " ^ grid_to_str sel_pce_loc)
  in
  let () =
    print_endline
      "Input destination location in EXACT format (row,column). Upper \
       left is (0,0) and bottom right is (7,7). NO SPACES!";
    print_endline "Or type 'reselect' to select a different piece.";
    print_string ">"
  in
  get_dest_loc brd state sel_pce_loc player_turn

(* ===================================================== *)

let rec get_input_dest brd st player_turn =
  let sel_pce_loc = select_piece st player_turn in

  (* Get the destination location and return a list with it and starting
     location.*)
  get_dest brd st sel_pce_loc player_turn

(* ===================================================== *)

let update_player_turn player_turn =
  if player_turn = "W" then "B" else "W"

(* ================================================================== *)
(* =======================END HELPER FUNCTIONS======================= *)
(* ================================================================== *)
(* ================================================================== *)

(* THE MAIN FUNCTION FOR GAMEPLAY. *)
let rec play_game brd st player_turn all_boards =
  let () = print_endline "Current board:" in
  let () = print_board brd in

  let t = state_to_list st in
  let locs = List.map (fun x -> fst x) t in
  let moves = possible_moves_list st t player_turn locs in

  let () = print_endline (list_to_string moves) in

  let is_in_check =
    in_check st (find_king st player_turn |> what_piece st)
  in
  let () =
    print_endline
      (player_turn ^ " in check: " ^ string_of_bool is_in_check)
  in
  (* Get [starting loc; dest loc; castle?] in list form *)
  let input_dest = get_input_dest brd st player_turn in

  (* Get starting position out of list *)
  let select_pce_loc =
    match List.hd input_dest with
    | Location x -> x
    | _ -> failwith "This should never be reached."
  in

  (* Get dest location out of list *)
  let dest =
    match input_dest |> List.tl |> List.hd with
    | Location x -> x
    | _ -> failwith "This should never be reached."
  in

  (* Get bool of if castling needed. *)
  let do_castle =
    match List.nth input_dest 2 with
    | Castle b -> b
    | _ -> failwith "This should never be reached."
  in

  (* Move piece(s) appropriately and return list of new board and
     state. *)
  let updated_game =
    match do_castle with
    | true -> complete_castle brd st select_pce_loc dest
    | false -> normal_move brd st select_pce_loc dest
  in

  (* Extract updated board from list. *)
  let brd =
    match List.hd updated_game with
    | Board b -> b
    | _ -> failwith "This should never be reached."
  in

  let all_boards = brd :: all_boards in

  (* Extract updated state from list. *)
  let st =
    match updated_game |> List.tl |> List.hd with
    | State s -> s
    | _ -> failwith "This should never be reached."
  in

  let () = print_endline "Move complete:" in
  let () = print_board brd in

  (* COULD CALL A FUNCTION AT THIS POINT TO CHECK FOR CHECKMATE. IF
     CHECKMATE, THEN THE GAME WOULD STOP AND A WINNER WOULD BE PRINTED
     OUT AT THIS POINT. *)
  let player_turn = update_player_turn player_turn in

  let brd = flip brd in
  let st =
    reset_en_passant st (get_color (what_piece st dest)) |> flip_state
  in
  if checkmate st player_turn then
    if player_turn = "W" then print_endline "Checkmate! Black Wins"
    else print_endline "Checkmate! White Wins"
  else if stalemate st player_turn || insufficient_material st then
    print_endline "Game Over. Stalemate."
  else
    let () = print_endline "" in
    let () = print_endline "Next player - board flipped: " in
    let () = print_board brd in
    let () =
      print_endline "";
      print_endline "Keep playing? y or n"
    in
    let keep_play = keep_playing () in
    if keep_play = "y" then play_game brd st player_turn all_boards
    else print_endline "Goodbye!"

(* ================================================================== *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Chess.\n";
  let board = init_board () in
  let st = init_state board in
  let player_turn = "W" in
  let all_boards : Board.t list = [] in
  play_game board st player_turn all_boards

(* Starts game. *)
let () = main ()