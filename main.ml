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
(* when you add an entry into the help file, add it as a (string *string) with
   the first string the command name to be matched against, and the second is
   the description. Make sure to add each command to the "Commands" entry which
   is first.*)
let helps = [
  ("commands",
"-Commands-
help - quit - roll - characters");

  ("help",
"-Help-
Help prints help for any command. Help alone prints help for every command.
Usage: help ___

Type \"help commands\" to get a list of commands.");

  ("quit",
   "-Quit-
Quit quits the interactive toolkit. Make sure you save first.
Usage: \"quit\" ");

  ("roll",
"-Roll-
Roll n1dn2tn3 rolls n1 dice of size n2 and takes n3 of them.
Usage: \"roll 3d6t2\" rolls three d6 dice and drops the lowest (taking the top two)."
  );
  ("characters",
   "-Characters-
Characters ___ gives you the list of characters in the current room. The
optional argument allows you to filter it by role.
Roles: Party, Neutral, Hostile, or Friendly.
Usage: \"characters ?role\"" );
]

let help_file = String.concat "\n \n" (List.map (fun x-> snd x) helps)

(* [repl s] updates the state of the adventure *)
let rec repl state =
  ANSITerminal.(print_string [red]  "\n> ");
  let cmd =  read_line () |> parse  in
  let s' = action cmd state  in
  if cmd = Invalid
  then
    let () = print_endline ("Invalid move. Try again. Type \"help commands\" for a list of commands.")
    in repl s' else
  let () = match cmd with
    |Quit -> exit 0
    |Help s -> begin try
      let print = if (s = "") then help_file else (List.assoc s helps) in
      ANSITerminal.(print_string [blue] print);
        with _->  print_endline ("No help file for " ^ s) end
    |Event ->  (*ANSITerminal.(print_string [green] "Current event is "^ s'.event);*)
      print_endline (State.gen_printout s');
      (*)|Inquiry -> *)
    |Move x-> (*ANSITerminal.(print_string [green] "You're now in "^ s'.current_location)*)
      print_endline (State.gen_printout s');
    | _ -> print_endline (State.gen_printout s');
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
