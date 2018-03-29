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

  (* [status character state] is a string containing the character's current status. *)
  val status :  c -> state -> string

  (* [strength character] is an int that describes the player's strength stat. *)
  val strength :  c -> int

  (* [dexterity character] is an int that describes the player's dexterity stat. *)
  val dexterity :  c -> int

  (* [intel character] is an int that describes the player's intelligence stat. *)
  val intel :  c -> int

  (* [strength character] is an int that describes the player's strength stat. *)
  val strength :  c -> int

  (* [speed character] is an int that describes the player's speed stat. *)
  val speed :  c -> int

  (* [hp character] is an int that contains the player's hit points . *)
  val hp :  c -> int

  (* [xp character] is an int that contains the player's experience points . *)
  val xp :  c -> int

  (* [skills character] is a list of skills a player has. *)
  val skills :  c -> skill list

  (* [abilities character] is a list of the abilities or spells a player can use. *)
  val abilities :  c -> ability list

  (* [inv character data] is a list of the items a player has. *)
  val inv :  c -> data -> string list
end
