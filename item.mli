open Database

module type Item = sig
  module D : Database

  (* [data] is the type of data in database.mli *)
  type data = D.data

  (* [i] is the type of an item. *)
  type i

(* [itemtype] is the variety of item, which determines where it is equipped. *)
  type itemtype = Weapon | Shield | Armor | Ring | Other

(* [make_item n d] is an item with the properties defined in [d] and
   name [n]. *)
  val make_item : string -> data -> i

(* [name i] is the name of the item. *)
  val name : i -> string

(* [item_type i] is the itemtype of [i]. *)
  val item_type : i -> itemtype

(* [description i] is the string description of the item *)
  val description : i -> string

(* [weight i] is the integer weight of the item *)
  val weight : i -> int

(* [passive_effects i] is the list of the names of effects caused by having
   the item equipped. *)
  val passive_effects : i -> string list

(* [use_effects i] is the list of the names of effects that are triggered by
  using the item.*)
  val use_effects : i -> string list

(* [use i] uses item i once, reducing the number of uses left.*)
  val use : i -> i

(* [set_uses n i] sets the number of uses left in item [i] to [n]*)
  val set_uses : int -> i -> i

(* [uses i] is the number of uses left for item [i]. If [uses] is 0, the
item can no longer be actively used.*)
  val uses_left : i -> int

(* [value i] is the value in currency of item [i]. *)
  val value : i -> int

(* [set_value n i] is a copy of [i] with the value [n]. *)
  val set_value : int -> i -> i

end
