(*
 * CS 3110 Spring 2018 Final Project
 * DM Toolkit
 * Authors: Allan Buchness, Kerri Diamond, Laasya Renganathan, Luke Ellert-Beck
 * NetID: ahb92, lpr46
 *
 *
 *
 *)
open State
open Command

(* [repl s] updates the state of the adventure *)
let rec repl state =
  print_string  "\n> "; ()

(* [play_game f] plays the game in adventure file [f]. *)
let start_game f =
  try ()
  with _ -> print_endline (" Invalid D&D File. Try again?")

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the DM toolkit\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start_game file_name

let () = main ()
