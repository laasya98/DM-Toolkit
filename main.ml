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
  ANSITerminal.(print_string [red]  "\n> ");
  let cmd =  read_line () |> parse  in
  let s' = action cmd state  in
  if cmd = Invalid
  then
    let () = print_endline ("Invalid move. Try again.") in repl s' else
  let () = match cmd with
    |Quit -> exit 0
    |Help -> ANSITerminal.(print_string [blue] "some rules and such");
    |Event x ->  (*ANSITerminal.(print_string [green] "Current event is "^ s'.event);*)
      print_endline s'.output;
      (*)|Inquiry -> *)
    |Move x-> (*ANSITerminal.(print_string [green] "You're now in "^ s'.current_location)*)
      print_endline (s'.output);
    |Use x -> print_endline (s'.output);
    |Give x-> print_endline (s'.output);
    |Take x-> print_endline (s'.output);
    |Drop x -> print_endline (s'.output);
    |Shop x-> print_endline (s'.output);
    |Buy x -> print_endline (s'.output);
    |Fight x -> print_endline (s'.output);
    |Cast x -> print_endline (s'.output);
    |Inv -> print_endline (s'.output);
    |Turn -> print_endline (s'.output);
    |QuickBuild x -> print_endline (s'.output);
    |CharacterAction -> print_endline (s'.output);
    |StateChange x -> print_endline (s'.output);
    |ItemChange x -> print_endline (s'.output);
    |Roll x -> print_endline (s'.output);
    |GetCharacterList ox -> print_endline (s'.output);
    |Invalid -> print_endline "Invalid Command";
    | _ -> ()
  in
repl s'

(* [play_game f] plays the game in adventure file [f]. *)
let start_game f =
  (repl empty_state)
(*with _ -> print_endline (" Invalid D&D File. Try again?")*)

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red] "\n\nWelcome to the DM toolkit\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start_game file_name

let () = main ()
