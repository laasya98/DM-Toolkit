open Database
open Event
open Character
open Command
open Item

(** A [State] is a representation of the state of the toolkit, including every
    current location and entity, and any statuses or active effects for the
    current gamespace.*)
module type State = sig
  module D : Database
  module C : Character
  module I : Item

(* [data] is the type of data in database.mli *)
  type data = D.data

(* [character] is the type of a character in character.mli *)
  type character = C.c

(* [item] is the type of an item in item.mli *)
  type item = I.i

(* [t] is the type of a state*)
  type t
end
