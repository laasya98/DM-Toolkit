open Database
open State

module type Character = sig
(* [D] is the signature of the game database. *)
  module D : Database

  module S : State

(* [state] is the type of State in state.mli *)
  type state = S.t

(* [data] is the type of data in database.mli *)
  type data = D.data

  (* type t is the character type. this should contain a record of information
     about a character in the game. this record can be updated by state and events *)
  type c

  (* type skill should be all the possible skill types a player can potentially have *)
  type skill

  (* type skill should be all the possible spells a player can potentially use *)
  type ability

  (* [name character] is a string containing the character's title. *)
  val name :  c -> string

  (* [status character ] is a string containing the character's current status. *)
  val status :  c -> string

  (* [update_state character ] updates the status of a character based off of the game state*)
  val update_status :  c -> state -> c

  (* [dexterity character] is an int that describes the player's dexterity stat. *)
  val dexterity :  c -> int

  (* [update_dex character new_s] character with dexterity = new_d. *)
  val update_dex :  c -> int -> c

  (* [intel character] is an int that describes the player's intelligence stat. *)
  val intel :  c -> int

  (* [update_intel character new_i] character with intel = new_i. *)
  val update_intel :  c -> int -> c

  (* [strength character] is an int that describes the player's strength stat. *)
  val strength :  c -> int

  (* [update_strength character new_s] character with strength = new_s. *)
  val update_strength :  c -> int -> c

  (* [speed character] is an int that describes the player's speed stat. *)
  val speed :  c -> int

  (* [update_speed character new_s] character with speed = new_s. *)
  val update_speed :  c -> int -> c

  (* [curr_hp character] is an int that contains the player's current hit points.*)
  val curr_hp :  c -> int

  (* [update_hp character new_hp] character with hp = new_hp.*)
  val update_hp :  c -> int -> c

  (* [max_hp character] is an int that contains the player's max possible hit points.*)
  val max_hp :  c -> int

  (* [xp character] is an int that contains the player's experience points . *)
  val xp :  c -> int

  (* [update_xp character new_xp] character with xp = new_xp. *)
  val update_xp :  c -> int -> c

  (* [level character] is an int that describes the character's current level . *)
  val level :  c -> int

  (* [level_up character lvl] increments character's level by lvl *)
  val level_up :  c -> int -> c

  (* [skills character] is a list of skills a player has. *)
  val skills :  c -> skill list

  (* [add_skill character skill] adds skill to character's skill list. *)
  val add_skill :  c -> skill -> c

  (* [abilities character] c with xp = new_xp. *)
  val abilities :  c -> ability list

  (* [add_skill character skill] adds ability to the abilities a character can perform. *)
  val add_ability :  c -> ability -> c

  (* [inv character data] is a list of the items a player has. *)
  val inv :  c -> data -> item list



end
