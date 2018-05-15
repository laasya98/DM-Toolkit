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
open Database
(* when you add an entry into the help file, add it as a (string *string) with
   the first string the command name to be matched against, and the second is
   the description. Make sure to add each command to the "Commands" entry which
   is first.*)
let helps = [
  ("commands",
"-Commands-
help - quit - roll - get - characters - exits - quickevent - battle");

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
  ("get",
"-Get-
Get [argument] gives you information about the argument.

  Accepted arguments:
  characters [role] gives a list of characters by role (Party, Neutral, Hostile, or Friendly).
  exits gives a list of directions one can move to in the current location.
    ");

  ("characters",
   "-Characters-
Characters gives you the list of characters in the current room.
Usage: \"characters ?role\"" );
  ("quickbuild",
   "-QuickBuild-
Quickbuild [name] [class] [race] creates a new character based off the three arguments,
and adds that character to the current party.
Type \"help races\" or \"help classes\" to get a list of the classes in the game.
Usage: \"quickbuild name class race\"" );
  ("races",
   "-Races-
  The list of races currently in the game are:
  | Dwarf
  | Elf
  | Halfling
  | Human
  | Dragonborn
  | Gnome
  | Half_Elf
  | Half_Orc
  | Tiefling" );
  ("classes",
   "-Classes-
  The list of classes currently in the game are:
  | Barbarian
  | Bard
  | Cleric
  | Druid
  | Fighter
  | Monk
  | Paladin
  | Ranger
  | Rogue
  | Sorcerer
  | Warlock
  | Wizard" );
  ("who is",
   "-Who Is?-
Who is [name] prints out the important stats about a player with name [name],
including the race, class, HP, and core stat values.
Usage: \"who is name \" or \"whomst name \"" );

  ("quickevent",
  "-QuickEvent-
Quickbuild [name] [form] creates a new event with those values, and starts it.
The list of acceptable event forms are:
  Battle: A combat event that tracks turn order and character attacks.
          Type \"help battle\" for more information about battles and combat.
  Shop:   A shop event wherein the player can buy and sell items.
          Type \"help battle\" for more information about shops.
  Interaction:  The default event. Neither a battle nor a shop.
          Use this to remove one of the other types.
Usage: \"quickbuild name form\"");

  ("battle",
   "-Battle-
Within a Battle event, some special actions can be taken.
  Fight: \"fight [attacker] [defender]\"
      Use this command to have the [attacker] character attempt to hit the
      [defender] character with their equipped weapon. If no weapon is equipped,
      an unarmed strike is made instead.
  Turn: \"turn\" moves the turn to the next player. Once all players have
      taken a turn, the turn number increases. This is used for spellcasting,
      to deal with wait times.");

  ("shop",
   "-Shop-
Within a Shop event, some special actions can be taken.
    Buy: \"buy [character] [item] [quantity]\"
        Use this command to have the [character] buy [quantity] of [item]
        from the shop. This will fail if the item is not available or the
        character doesn't have enough money.
    Sell: \"sell [character] [item] [quantity]\"
        Use this command to have the [character] sell [quantity] of [item]
        to the shop. This will fail if the item is not owned by the character.")
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
      ANSITerminal.(print_string [blue] (print ^ "\n"));
        with _->  print_endline ("No help file for " ^ s ^ "\n") end
    |Event ->  (*ANSITerminal.(print_string [green] "Current event is "^ s'.event);*)
      print_endline ((State.gen_printout s'));
      (*)|Inquiry -> *)
    |Move x-> (*ANSITerminal.(print_string [green] "You're now in "^ s'.current_location)*)
      print_endline ((State.gen_printout s'));
    | _ -> print_endline ((State.gen_printout s'));
  in
    repl s'

(* [play_game f] plays the game in adventure file [f]. *)
let rec start_game f =
  if (String.lowercase_ascii f = "quit") then
    exit 0
  else
  try
    Database.change_file "state" f;
    let d = init_state "state" in
    repl d
  with x ->
      begin
        ANSITerminal.(print_string [red] "Not a valid file.\n");
        print_endline "Please enter the name of the game file you want to load.\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> start_game "QUIT"
        | file_name -> start_game file_name
                end


(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red] "\n\nWelcome to the DM toolkit for Dungeons and Dragons!\n\n

                                                 __----~~~~~~~~~~~------___
                                     .  .   ~~//====......          __--~ ~~
                   -.              \\_|//     |||\\\\  ~~~~~~::::... /~
                ___-==_       _-~o~  \\/    |||  \\\\            _/~~-
       __---~~~.==~||\\=_    -_--~/_-~|-   |\\\\   \\\\        _/~
   _-~~     .=~    |  \\\\-_    '-~7  /-   /  ||    \\      /
 .~       .~       |   \\\\ -_    /  /-   /   ||      \\   /
/  ____  /         |    \\ \\ ~-_/  /|- _/   .||       \\ /
|~~    ~~|--~~~~--_ \\     ~==-/   | \\~--===~~        .\\
           '         ~-|      /|    |-~\\~~       __--~~
                       |-~~-_/ |    |   ~\\_   _-~            /\\
                            /  \\     \\__   \\/~                \\__
                        _--~ _/ | .-~~____--~-/                  ~~==.
                      ((->/~   '.|||' -_|    ~~-/ ,              . _||
                                 -_     ~\\      ~~---l__i__i__i--~~_/
    _|   _    _|             _-~-__   ~)  \\--______________--~~
   |_|  | |  |_|          //.-~~~-~_--~- |-------~~~~~~~~
                                      //.-~~~--\



");
  print_endline "Please enter the name of the game file you want to load.\n
                 (def for default file)";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> if file_name = "def" then start_game "data/teststate.csv"
                 else start_game file_name

let () = main ()
