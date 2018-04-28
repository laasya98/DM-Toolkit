open Character
open Global

module type Event = sig
  module C : Character
  type character = Character.c
  type t
  type form = Battle | Shop | Interaction
  type role = Hostile | Friendly | Neutral
  type npc = {details:character; role:role; tag:int}

  val make_event : string -> form -> npc list -> item list -> t
  val get_form : t -> form
  val get_name : t -> string
  val get_output : t -> string
  val add_npc : character -> role -> t -> t
  val remove_npc : int -> t -> t
  val update_npc : npc -> ?c:character -> ?r:role -> ?o:string -> t -> t
  val get_npcs : t -> npc list
  val add_item : item -> t -> t
  val remove_item : string -> t -> t
  val get_items : t -> item list
  val change_form : form -> t -> t
  val attack_opt : character option -> string -> character option -> string -> t
    -> (t * character option)
end

module Event = struct
    module C = Character

  type character = Character.c

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
    tagNum: int;
    output: string;
  }

  let make_event name form npcs items =
    {
      name=name;
      form = form;
      npcs = npcs;
      items = items;
      tagNum = 0;
      output = "Event created.";
    }

  let alter_event evt ?(name=evt.name) ?(form=evt.form) ?(npcs=evt.npcs)
      ?(tag=evt.tagNum) ?(items = evt.items) output =
    {
      name=name;
      form = form;
      npcs = npcs;
      items = items;
      tagNum = tag;
      output = output;
    }

  let get_form evt = evt.form

  let get_name evt = evt.name

  let get_output evt = evt.output

  (* [add_npc name d role evt] adds an npc with name [name] parsed from data [d]
     with a [role] that determines its actions within the event.
     If [d] is invalid, return [evt]. *)
  let add_npc c role evt =
    let npc = {details=c; role=role; tag=evt.tagNum} in
    alter_event evt ~npcs:(npc::evt.npcs) ~tag:(evt.tagNum + 1) "NPC added."

  (* [remove_npc tag evt] removes the npc with tag [tag] from [evt], and returns
     the new [evt]. If there is no npc of that tag, return [evt]. *)
  let remove_npc tag evt =
    let npcs = List.filter (fun x -> x.tag <> tag) evt.npcs in
    alter_event evt ~npcs:(npcs) "NPC removed."

  let get_npcs evt = evt.npcs

  let npc_by_name n evt =
    List.find_opt (fun x -> C.name x.details = n) evt.npcs

  let update_npc npc ?(c=npc.details) ?(r=npc.role) ?(o="") evt =
    let n = {details=c; role = r; tag = npc.tag} in
    let npcs = List.map (fun x -> if x.tag = npc.tag then n else x) evt.npcs in
    alter_event evt ~npcs:(npcs) (o^"NPC status updated.")

  let add_item i evt =
    try
      let items =  i::evt.items in
      alter_event evt ~items:(items) "Item added."
    with _ ->
      alter_event evt "Item could not be added."

        (*TODO: remove just one if duplicates? *)
  let remove_item name evt =
    let items = List.filter (fun (i:item) -> i.name <> name) evt.items in
    alter_event evt ~items:(items) "Item removed."

  let get_items evt = evt.items

  let change_form form evt = alter_event evt ~form:(form) "Form changed."

  (*TODO: get the EQUIPPED weapon. *)
  let get_weapon c =
    let weapon =List.find (fun x ->
        match x.i_type with | Weapon _ -> true |_-> false) (C.equipped c) in
    match weapon.i_type with
    | Weapon w -> w
    | _ -> failwith "No weapon"

  (*TODO: get the EQUIPPED armor. *)
  let get_armor c =
    let armor = List.find
        (fun x -> match x.i_type with | Armor _ -> true | _-> false)
        (C.equipped c)
    in
    match armor.i_type with
    | Armor a -> a
    | _ -> failwith "No armor"

  let attack_roll attacker target =
    let d20 = 1+ Random.int 19 in
    let ability =
      try
        if (get_weapon attacker).t = Ranged then C.dex attacker
        else C.strength attacker
      with _ -> C.strength attacker
    in
    let prof = 0 in
    let ac = try (get_armor target).ac with _ -> 0 in
    if d20 = 1 then 0
    else if d20 = 20 then 2
    else
      let roll = d20 + ability + prof in
      if roll > ac then 1 else 0

  let rec roll_dice dice acc =
    match dice with
    | [] -> acc
    | h::t -> roll_dice t (acc + 1 + Random.int h)

  let damage_roll attacker crit =
    let dice = try
        let weapon = get_weapon attacker in
        if crit then weapon.dice @ weapon.dice
        else weapon.dice
      with _ -> [] in
    let ability = try
      if (get_weapon attacker).t = Ranged then C.dex attacker
      else C.strength attacker
    with _ -> C.strength attacker in
      ability + roll_dice dice 0

  let deal_damage amount target =
    let hp = C.curr_hp target in
    C.update_hp target (hp - amount)

  (* returns the new state of the target after being hit*)
  let attack_t attacker target =
    let hit = attack_roll attacker target in
    if hit = 0 then target else
      deal_damage (damage_roll attacker (hit=2)) target

  let attack_npc a t evt =
    let t' = attack_t a t.details in
    let out = (C.name t.details)^" was attacked. " in
    update_npc t ~c:t' ~o:out evt

  let attack_opt a an t tn evt =
    let at = match a with
      | None -> begin
          match npc_by_name an evt with
          | None -> failwith "C not found"
          | Some c -> c.details
        end
      | Some c -> c
    in
    match t with
    | None -> begin
        match npc_by_name tn evt with
        | None -> failwith "C not found"
        | Some t -> (attack_npc at t evt, None)
    end
    | Some c -> (evt, Some (attack_t at c))

end
