open Chess
open Board
open State
open Pieces

(* This works in terminal only. To play type "make play" in terminal.
   Note you may have issues if there is a problem in files in src/.*)

let str_to_grid str =
  (int_of_char str.[1] - 48, int_of_char str.[3] - 48)

let rec can_move brd piece dest =
  match is_legal piece dest with
  | true -> dest
  | false ->
      let () =
        print_endline
          "Cannot move to that location. Input a new destination \
           location."
      in
      let new_dest = read_line () |> str_to_grid in
      can_move brd piece new_dest

let rec check_input state input =
  let piece = what_piece state input in
  match is_legal piece input with
  | true
  | false ->
      input
  | exception Illegal x ->
      let () =
        print_endline
          "Not valid piece. Input location of piece you want to select."
      in
      let new_selection = read_line () |> str_to_grid in
      check_input state new_selection

let rec play_game brd =
  let state = get_state brd in
  let () = print_endline "Current board:" in
  let () = print_board brd in
  let () =
    print_endline
      "Input the location of the PIECE YOU WANT TO MOVE in EXACT \
       format (row,column). Upper left is (0,0) and bottom right is \
       (7,7). NO SPACES!"
  in
  let input = read_line () |> str_to_grid in
  let input = check_input state input in
  let () =
    print_endline
      "Input the location TO WHICH YOU WANT TO MOVE that piece in \
       EXACT format (row,column). Upper left is (0,0) and bottom right \
       is (7,7). NO SPACES!"
  in
  let dest = read_line () |> str_to_grid in
  let piece = what_piece state input in
  let dest = can_move brd piece dest in
  (* update board *)
  let brd = move_piece brd input dest in
  let () = print_endline "Move complete:" in
  let () = print_board brd in
  let brd = flip brd in
  let () = print_endline "" in
  let () = print_endline "Next player - board flipped: " in
  let () = print_board brd in
  let () =
    print_endline "";
    print_endline "Keep playing? y or n"
  in
  let keep_play = read_line () |> String.lowercase_ascii in
  if keep_play = "y" then play_game brd else print_endline "Goodbye"

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Extremely Simple Chess.\n";
  let board = init_board () in
  let play = play_game board in
  play

let () = main ()

(*note to self - state when board flips - need to consider! *)
