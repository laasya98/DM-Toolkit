open Database
open Character
open Item

module type Event = sig
  module D : Database
  module C : Character
  module I : Item
  type data = D.data
  type character = C.c
  type item = I.i
  type t
  type form = Battle | Shop | Interaction
  type role = Hostile | Friendly | Neutral
  type npc = {details:character; role:role; tag:int}
  val make_event : string -> form -> data option -> t
  val get_form : t -> form
  val get_name : t -> string
  val save_event : t -> data
  val add_npc : string -> data -> role -> t -> t
  val remove_npc : int -> t -> t
  val get_npcs : t -> npc list
  val add_item : string -> data -> t -> t
  val remove_item : string -> t -> t
  val get_items : t -> item list
  val change_form : form -> t -> t
end

module MakeEvent
  = functor (D:Database) -> functor (C:Character) -> functor (I:Item) ->
struct
  type data = D.data

  type character = C.c

  type item = I.i

  (* [form] is the variety of an event. *)
  type form = Battle | Shop | Interaction

  (* [role] is the role of an npc.
     A Friendly npc will aid the characters in the event
     A Hostile npc will oppose the characters in the event
     A Neutral npc will do neither (ex: shopkeeper). *)
  type role = Hostile | Friendly | Neutral

  (* [npc] is the type of an npc. It wraps the Character type with a role and
     a tag. *)
  type npc = {details:character; role:role; tag:int}

  type t = {
    name: string;
    form:form;
    npcs: npc list;
    items: item list;
    output: string;
  }

  (* [make_event name form d] creates an event of type [form] with the name [name]
     and properties defined by data option [d]. If [d] is None, a default event
     of that type is returned. *)
  let make_event name form d = failwith "unimplemented"

  let alter_event evt ?(name=evt.name) ?(form=evt.form) ?(npcs=evt.npcs)
      ?(items = evt.items) output =
    {
      name=name;
      form = form;
      npcs = npcs;
      items = items;
      output=output;
    }

  let get_form evt = evt.form

  let get_name evt = evt.name

  (* [save_event evt] creates a datatype object with the properties of
     the event *)
  let save_event evt = failwith "unimplemented"

  (* [add_npc name d role evt] adds an npc with name [name] parsed from data [d]
     with a [role] that determines its actions within the event.
     If [d] is invalid, return [evt]. *)
  let add_npc name d role evt = failwith "unimplemented"

  (* [remove_npc tag evt] removes the npc with tag [tag] from [evt], and returns
     the new [evt]. If there is no npc of that tag, return [evt]. *)
  let remove_npc tag evt = failwith "unimplemented"

  let get_npcs evt = evt.npcs

  let add_item name d evt =
    try
      let items = (I.make_item name d)::evt.items in
      alter_event evt ~items:(items) "Item added."
    with _ ->
      alter_event evt "Item could not be added due to an error in the file."

  let remove_item name evt =
    let items = List.filter (fun i -> (I.name i) <> name) evt.items in
    alter_event evt ~items:(items) "Item removed."

  let get_items evt = evt.items

  let change_form form evt = alter_event evt ~form:(form) "Form changed."
end
