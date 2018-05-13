open Character
open Global

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
    v_out: string;
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
      v_out = "";
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
      v_out = "";
      output = "Event created.";
    }

  let alter_event evt ?(name=evt.name) ?(form=evt.form) ?(items = evt.items)
      ?(turn = evt.turn) ?(t_order = evt.turn_order)
      ?(spells = evt.spells) ?(v=evt.v_out) output =
    {
      name=name;
      form = form;
      items = items;
      turn = turn;
      turn_order = t_order;
      v_out = v;
      output = output;
      spells = spells;
    }

let clear_vout evt = alter_event evt ~v:"" "Verbose cleared."

let add_vout s evt = alter_event evt ~v:(evt.v_out^"\n"^s) "Verbose updated."

let rec add_vout_lst s lst evt =
  match lst with
  | [] -> add_vout s evt
  | h::t -> add_vout_lst (s^"\n"^h) t evt

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

(*TODO: don't change bases? *)
let apply_effect s a t =
  match s with
  | Constitution -> C.update_const t (C.const t + a)
  | Charisma -> C.update_charisma t (C.charisma t + a)
  | Wisdom -> C.update_wisdom t (C.wisdom t + a)
  | Intel -> C.update_intel t (C.intel t + a)
  | Str -> C.update_strength t (C.strength t + a)
  | Dex -> C.update_dex t (C.dex t + a)
  | HP -> C.update_hp t (C.curr_hp t + a)

let deal_damage a t = apply_effect HP (-1 *a) t

(* returns the event and new state of the target after being hit*)
  let attack a t evt =
    let hit = attack_roll a t in
    if hit = 0 then (add_vout "Attack missed." evt, t)
    else if hit=2 then
      let d = damage_roll a true in
      let v = (C.name a)^" critically hit "^(C.name t)
              ^" for "^(string_of_int d)^" damage." in
      (add_vout v evt, deal_damage d t)
    else
      let d = damage_roll a false in
      let v = (C.name a)^" hit "^(C.name t)
              ^" for "^(string_of_int d)^" damage." in
      (add_vout v evt, deal_damage d t)
(*TODO: add test for death, turn #, items becoming available, xp gain, etc
*)

  (* [0 1 (2) 3] *)
  let get_turnlst evt = evt.turn_order

  let get_turn evt = evt.turn

  let get_waiting_spells evt =
    List.map (fun x -> (x.spell, x.turn)) evt.spells

  let add_spell c s t evt =
    let turn = evt.turn + s.to_cast in
    let spell = {turn=turn; castor=c; spell=s; targets=t} in
    let v = (C.name c)^" began casting "^s.name^"! It will cast in "^
            (string_of_int s.to_cast)^" turns." in
    alter_event evt ~spells:(spell::evt.spells) ~v:v "Spell timer added."

(* [count_dups lst] is the list of tuples of each element of the list and
   how many times it appeared in the original list. It contains no duplicate
   elements.
   Ex: [count_dups [1;2;1;1]] is [(1,3);(2,1)]*)
let count_dups lst =
  let rec count acc lst =
    match lst with
    | [] -> acc
    | h::t ->
      let t' = List.filter (fun x -> x<>h) t in
      let n = List.length t - List.length t' in
      let acc' = (h, n+1)::acc in
      count acc' t'
  in
  count [] lst

let spell_damage s (t,n) =
  let rec dam s n acc =
    if n>0 then
      let d = roll_dice s.damage_die s.bonus_damage in
      dam s (n-1) (acc+d)
    else
      acc
  in
  let d = dam s n 0 in
  let v = (C.name t)^" took "^(string_of_int d)^" damage!" in
  (deal_damage d t, v)

let cast_damage c s t evt =
  if s.multiple then
    let t' = count_dups t in
    List.map (fun n -> spell_damage s n) t'
  else
    let d = roll_dice s.damage_die s.bonus_damage in
    List.map (fun n -> (deal_damage d n, (C.name n)^" took "^
                                         (string_of_int d)^" damage!")) t

let string_of_stat s =
  match s with
  | Constitution -> "Constitution"
  | Charisma -> "Charisma"
  | Wisdom -> "Wisdom"
  | Intel -> "Intelligence"
  | Str -> "Strength"
  | Dex -> "Dexterity"
  | HP -> "HP"

let cast_status c s t evt =
  let d = roll_dice s.die s.bonus in
  List.map (fun n -> (apply_effect s.stat d n,
                      (C.name n)^"'s "^(string_of_stat s.stat)^" changed by "^
                      (string_of_int d)^"!")) t

    (*TODO: consolidate*)
let use_item (i:item) c evt =
  let v = (C.name c)^" used "^i.name^"!" in
  let c = cast_status c i.effect [c] (add_vout v evt) in
  let t' = List.map (fun x -> fst x) c in
  let v' = List.map (fun x -> snd x) c in
  let evt' = add_vout_lst "" v' evt in
  (evt', t')

  let cast c s t evt =
    if s.to_cast = 0 || evt.form <> Battle then
      match s.stype with (*TODO*)
      | Damage d -> begin
          let v = (C.name c)^" cast "^s.name^"!" in
          let c = cast_damage c d t (add_vout v evt) in
          let t' = List.map (fun x -> fst x) c in
          let v' = List.map (fun x -> snd x) c in
          let evt' = add_vout_lst "" v' evt in
          (evt', t')
      end
      | Conjuration -> (evt, [])
      | Status d -> begin
          let v = (C.name c)^" cast "^s.name^"!" in
          let c = cast_status c d t (add_vout v evt) in
          let t' = List.map (fun x -> fst x) c in
          let v' = List.map (fun x -> snd x) c in
          let evt' = add_vout_lst "" v' evt in
          (evt', t')
      end
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
