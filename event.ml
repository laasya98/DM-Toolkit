open Character
open Global

module type Event = sig
  module C : Character
  type character = Character.c
  type t
  type form = Battle | Shop | Interaction

  val init_event : string -> t
  val make_event : string -> form -> (item * quantity) list -> string list -> t
  val get_form : t -> form
  val get_name : t -> string
  val get_output : t -> string
  val add_item : item -> quantity -> t -> t
  val remove_item : string -> quantity -> t -> t
  val get_items : t -> (item * quantity) list
  val change_form : form -> t -> t
  val attack: character -> character -> t -> (t * character)
  val turn : t -> (t * character list)
  val get_turn : t -> int
  val get_turnlst : t -> string list
  val cast : character -> spell -> character list -> t -> (t * character list)
  val get_waiting_spells : t -> (spell*int) list
end

module Event = struct
  module C = Character

  type character = Character.c

  type form = Battle | Shop | Interaction
  type spellTimer = {turn:int; castor: character; spell: spell;
                     targets: character list}

  type t = {
    name: string;
    form: form;
    items: (item * quantity) list;
    turn: int;
    turn_order: string list;
    spells: spellTimer list;
    output: string;
  }

  let make_event name form items turn_order =
    {
      name=name;
      form = form;
      items = items;
      turn = 0;
      turn_order = turn_order;
      spells=[];
      output = "Event created.";
    }

  let init_event name =
    {
      name=name;
      form = Interaction;
      items = [];
      turn = 0;
      turn_order = [];
      spells=[];
      output = "Event created.";
    }

  let alter_event evt ?(name=evt.name) ?(form=evt.form) ?(items = evt.items)
      ?(turn = evt.turn) ?(t_order = evt.turn_order)
      ?(spells = evt.spells) output =
    {
      name=name;
      form = form;
      items = items;
      turn = turn;
      turn_order = t_order;
      output = output;
      spells = spells;
    }

  let get_form evt = evt.form

  let get_name evt = evt.name

  let get_output evt = evt.output

  let change_item_q i q lst =
    let gz n = match n with
      |Infinity -> true
      |Int n -> n>0
    in
    List.map (fun (a,b) -> if a=i then (a,q) else (a,b)) lst
    |> List.filter (fun (_,n) -> gz n)

  let add_item i q evt =
    match List.find_opt (fun (x,q) -> x=i) evt.items with
    | None -> alter_event evt ~items:((i,q)::evt.items) "Item added."
    | Some (x,n) ->
      match n with
      | Infinity -> alter_event evt "There are already infinite."
      | Int n ->
        match q with
        | Int q -> alter_event evt
                     ~items:(change_item_q i (Int (n+q)) evt.items) "Item added."
        | Infinity -> alter_event evt
                     ~items:(change_item_q i (Infinity) evt.items) "Infinite added."

  let remove_item name q evt =
    match List.find_opt (fun ((x:item),q) -> x.name=name) evt.items with
    | None -> alter_event evt "Item not present."
    | Some (x,n) ->
      match n with
      | Infinity -> if q=Infinity then alter_event evt
            ~items:(change_item_q x (Int 0) evt.items) "Infinite removed."
        else
          alter_event evt "There are infinite remaining."
      | Int n ->
        match q with
        | Int q -> alter_event evt
                    ~items:(change_item_q x (Int (n-q)) evt.items) "Item removed."
        | Infinity -> alter_event evt
                        ~items:(change_item_q x (Int 0) evt.items) "Infinite removed."

  let get_items evt = evt.items

  let change_form form evt = alter_event evt ~form:(form) "Form changed."

  let get_weapon c =
    let equipment = List.fold_left (fun n (a,b) -> a::n) [] (C.equipped c) in
    let weapon =List.find (fun x ->
        match x.i_type with | Weapon _ -> true |_-> false) equipment in
    match weapon.i_type with
    | Weapon w -> w
    | _ -> failwith "No weapon"

  let get_armor c =
    let equipment = List.fold_left (fun n (a,b) -> a::n) [] (C.equipped c) in
    let armor = List.find
        (fun x -> match x.i_type with | Armor _ -> true | _-> false)
        (equipment)
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

(*TODO: add test for death, turn #, items becoming available, xp gain, etc

  turn order List
  turn command
  timers for shit
*)
  let attack a t evt =
    (evt, attack_t a t)

  (* [0 1 (2) 3] *)
  let get_turnlst evt = evt.turn_order

  let get_turn evt = evt.turn

  let get_waiting_spells evt =
    List.map (fun x -> (x.spell, x.turn)) evt.spells

  let add_spell c s t evt =
    let turn = evt.turn + s.to_cast in
    let spell = {turn=turn; castor=c; spell=s; targets=t} in
    alter_event evt ~spells:(spell::evt.spells) "Spell timer added."

  let spell_damage s t =
    let d = roll_dice s.damage_die s.bonus_damage in
    deal_damage d t

  let cast_damage c s t evt =
    List.map (fun n -> spell_damage s n) t

  let cast c s t evt =
    if s.to_cast = 0 then
      match s.stype with (*TODO*)
      | Damage d -> (evt, [])
      | Conjuration -> (evt, [])
      | Transmutation -> (evt, [])
    else
      (add_spell c s t evt, [])

  let rec update_and_cast c s t evt (upc:character list) =
    match upc with
    | [] -> cast c s t evt
    | h::tl -> if h.name = c.name then
        update_and_cast h s t evt tl
      else
        let t' =
          List.map (fun (x:character) -> if x.name = h.name then h else x) t
        in
        update_and_cast c s t' evt tl

(* [castAll s (e, tar)] casts all spells in s with starting event e and list
   of altered characters cs
   Note that update_chars will process tar before t' so dups here don't matter.
*)
  let rec castAll s (e, tar) =
    match s with
    | [] -> (e,tar)
    | h::t -> let (e', t') = update_and_cast h.castor h.spell h.targets e tar in
    castAll t (e', tar @ t')

  let turn evt =
    let t' = evt.turn + 1 in
    let tlst = match evt.turn_order with
      |[] -> []
      |h::t -> t @ [h]
    in
    let spells = List.filter (fun (s:spellTimer) -> s.turn = t') evt.spells in
    let rs = List.filter (fun (s:spellTimer) -> s.turn <> t') evt.spells in
    let (evt', tar') = castAll spells (evt, []) in
    let evt'' = alter_event evt' ~spells:rs
        ~turn:t' ~t_order:tlst "Turn incremented."
    in
    (evt'', tar')

end
