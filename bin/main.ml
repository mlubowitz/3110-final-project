open Chess
open Board
open State
open Pieces

(* To play type "make play" in terminal.*)

(*Converts str "(0,0)" to a tuple (0,0). *)
let str_to_grid str =
  (int_of_char str.[1] - 48, int_of_char str.[3] - 48)

(* ==================================================================== *)

(* going to be difficult with move_piece if this doesn't return a
   location *)
(* is the destination location that piece at input can move to *)
let rec can_move brd state input =
  let dest = read_line () |> str_to_grid in
  let f_piece = what_piece state input in
  let dest_piece = what_piece state dest in
  (* let () = print_endline dest_piece.position in *)
  match
    is_legal f_piece dest_piece && is_path_empty state input dest
  with
  | true -> dest
  | false ->
      let () =
        print_endline
          "Cannot move to that location. Input a new destination \
           location."
      in
      can_move brd state input

(* ==================================================================== *)

(* checks if input location has a piece *)
let rec get_input state =
  let input = read_line () |> str_to_grid in
  let piece = what_piece state input in
  match no_piece piece with
  | false -> input
  | true ->
      let () =
        print_endline
          "Not valid piece. Input location of piece you want to select."
      in
      get_input state

(* ==================================================================== *)

let rec play_game brd state =
  let () = print_endline "Current board:" in
  let () = print_board brd in
  (* prompt for choosing piece player wants to move *)
  let () =
    print_endline
      "Input the location of the PIECE YOU WANT TO MOVE in EXACT \
       format (row,column). Upper left is (0,0) and bottom right is \
       (7,7). NO SPACES!"
  in
  (* get the location of the piece player wants to move. If location has
     no piece, keep asking until player inputs location with piece. *)
  let input = get_input state in
  (*this could be a piece type*)
  (* prompt for destination location *)
  let () =
    print_endline
      "Input the location TO WHICH YOU WANT TO MOVE that piece in \
       EXACT format (row,column). Upper left is (0,0) and bottom right \
       is (7,7). NO SPACES!"
  in
  (* get a destination location - check to see if move can be made from
     starting to destination loation; if not, keep asking until valid
     location is added *)
  let dest = can_move brd state input in
  (* move piece and update board *)
  let brd = move_piece brd input dest in
  let () = print_endline "Move complete:" in
  let () = print_board brd in
  let brd = flip brd in
  let state = flip_state state in
  let () = print_endline "" in
  let () = print_endline "Next player - board flipped: " in
  let () = print_board brd in
  let () =
    print_endline "";
    print_endline "Keep playing? y or n"
  in
  let keep_play = read_line () |> String.lowercase_ascii in
  if keep_play = "y" then play_game brd state
  else print_endline "Goodbye"

(* ==================================================================== *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Extremely Simple Chess.\n";
  let board = init_board () in
  let state = init_state board in
  play_game board state

let () = main ()

(*note to self - state when board flips - need to consider! *)
