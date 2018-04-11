open Character
open Global

module type Event = sig
  module C : Character
  type character = C.c
  type t
  type form = Battle | Shop | Interaction
  type role = Hostile | Friendly | Neutral
  type npc = {details:character; role:role; tag:int}
  val get_form : t -> form
  val get_name : t -> string
  val add_npc : string -> character -> role -> t -> t
  val remove_npc : int -> t -> t
  val get_npcs : t -> npc list
  val add_item : string -> item -> t -> t
  val remove_item : string -> t -> t
  val get_items : t -> item list
  val change_form : form -> t -> t
end


module MakeEvent
  = functor (C:Character) ->
struct
  type character = C.c

  type form = Battle | Shop | Interaction

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

  (* [add_npc name d role evt] adds an npc with name [name] parsed from data [d]
     with a [role] that determines its actions within the event.
     If [d] is invalid, return [evt]. *)
  let add_npc name c role evt = failwith "unimplemented"

  (* [remove_npc tag evt] removes the npc with tag [tag] from [evt], and returns
     the new [evt]. If there is no npc of that tag, return [evt]. *)
  let remove_npc tag evt = failwith "unimplemented"

  let get_npcs evt = evt.npcs

  let add_item i evt =
    try
      let items =  i::evt.items in
      alter_event evt ~items:(items) "Item added."
    with _ ->
      alter_event evt "Item could not be added due to an error in the file."

  let remove_item name evt =
    let items = List.filter (fun (i:item) -> i.name <> name) evt.items in
    alter_event evt ~items:(items) "Item removed."

  let get_items evt = evt.items

  let change_form form evt = alter_event evt ~form:(form) "Form changed."
end
