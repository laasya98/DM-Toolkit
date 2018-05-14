open Global
open Database

  type item = Global.item
  type skill = {
    name:string;
    prof:bool;
    modifier:int;
    corestat:string;
  }
  type spell = Global.spell

  type c_class =
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
    | Wizard

  type race =
    | Dwarf
    | Elf
    | Halfling
    | Human
    | Dragonborn
    | Gnome
    | Half_Elf
    | Half_Orc
    | Tiefling

type c = {
  name:string;
  race:race;
  c_class:c_class;
  (*alignment:Global.alignment;*)

  armor_class: int ;
  prof_bonus: int;
  passive_wisdom: int;

  constitution: int;
  cons_mod: int;
  charisma: int;
  char_mod: int;
  wisdom:int;
  wis_mod:int;
  intel:int;
  int_mod:int;
  strength:int;
  str_mod:int;
  dexterity:int;
  dex_mod:int;

  speed:int;
  max_hp:int;
  hp:int;

  hd:int;
  hd_qty:int;

  xp:int;
  level:int;

  skills: skill list;
  spells: spell list;
  equipped: ((item * quantity) list )* int;
  inv: ((item * quantity) list )* int;
  money: int;
}

  let name  c = c.name
  let race  c = c.race
  let class_of  c = c.c_class
  let wisdom  c = c.wisdom
  let update_wisdom  c w = {c with wisdom = w}
  let const  c = c.constitution
  let update_const  c o = {c with constitution = o}
  let armor_class  c = c.armor_class
  let update_ac c a = {c with armor_class = a;}
  let dex  c = c.dexterity
  let update_dex c d = {c with dexterity = d;}
  let intel c = c.intel
  let update_intel c i = {c with intel = i;}
  let strength c = c.strength
  let update_strength c s = {c with strength = s}
  let speed c = c.speed
  let update_speed c s = {c with speed = s}
  let curr_hp c = c.hp
  let update_hp c h =
    if c.max_hp < h then c else
    {c with hp = h}
  let max_hp c = c.max_hp
  let update_max_hp c h =
    {c with max_hp = h}
  let xp c = c.xp
  let update_xp c x = {c with xp = x}
  let level  c = c.level
  let level_up  c l =
    {c with
      level = l
    }
  (* TODO: whatever algorithm updates strength/skills/speed based off of level/chartype*)
  let skills c =  c.skills
  let spells c = c.spells
  let add_spell c s =
    let spells = c.spells in
    {c with spells = s::spells}
  let inv c : (item*quantity) list = fst c.inv
  let equipped c : (item*quantity) list= fst c.equipped

  let insert_qty i n l =
    if List.mem_assoc i l then
      let f (a,b) =
        if i=a then
          match b with
          |Infinity -> (a,b)
          |Int b' -> (a, Int (b'+n))
        else
          (a,b)
      in
      List.map f l else
      (i, Int n)::l

  (*let remove_qty i n l =
    if List.mem_assoc i l then
      List.map(fun (a,b) -> if i = a then (i,b-n) else (a,b)) l else
      l *)

  let equip c e n =
    let equipment =  fst (c.equipped) in
    let cap = snd c.equipped in
    if List.length equipment <= cap && List.mem_assoc e (fst (c.inv)) then
      {c with equipped = (insert_qty e n equipment),cap} else c
  let add_item c i n =
    let items = fst (c.inv) in
    let cap = snd c.inv in
    if List.length items <= cap then {c with inv = (insert_qty i n items), cap}
    else c
  let remove_item c i n = failwith "Unimplemented"
  let money c = c.money
  let update_money c m = {c with money = m}
  let charisma c = c.charisma
  let update_charisma c a = {c with charisma = a}


  let class_of_string s =
    match s |> String.lowercase_ascii  |> String.trim with
    | "barbarian" -> Barbarian
    | "bard" -> Bard
    | "cleric" -> Cleric
    | "druid" -> Druid
    | "fighter" -> Fighter
    | "monk" -> Monk
    | "paladin" -> Paladin
    | "ranger" -> Ranger
    | "rogue" -> Rogue
    | "sorcerer" -> Sorcerer
    | "warlock" -> Warlock
    | "wizard" -> Wizard
    | _ -> failwith "don't do that"

  let race_of_string s =
    match s |> String.lowercase_ascii  |> String.trim with
      | "dwarf" -> Dwarf
      | "elf" -> Elf
      | "halfling" -> Halfling
      | "human" -> Human
      | "dragonborn" -> Dragonborn
      | "gnome" -> Gnome
      | "half-elf" | "halfelf" -> Half_Elf
      | "halforc" | "half-orc" -> Half_Orc
      | "tiefling" -> Tiefling
      | _ -> failwith "not a race, spell better pls"

  let rec roll_dice (dice:int list) (acc : int list)  =
    match dice with
    | [] -> acc
    | h::t -> roll_dice t ((1 + (Random.int h))::acc)

  let sum lst =
    List.fold_left (fun a x -> x + a) 0 lst

  let minof lst : int=
    List.fold_left (fun a b -> if a <= b then a else b) (List.hd lst) lst

  let remove_min (lst : int list) : int list=
    let minval = minof lst in
      List.filter (fun x -> (x != minval)) lst

  let rec stat_lister acc : int list=
    if List.length acc = 6
      then
        acc |> List.sort compare |> List.rev
      else
        let stat =  roll_dice [6;6;6;6] []  |> remove_min |> sum in
        stat_lister (stat::acc)

let ability_mod a =
  let b = a - 10 in
  if (b/2)*2 = b
  then b/2 else (b-1)/2

let all_skills = (*Database.allskills*)
  {
    name="acrobatics";
    prof=false;
    modifier=0;
    corestat = "dex";
  } ::
  {
    name="arcana";
    prof=false;
    modifier=0;
    corestat = "int";
  } ::
  {
    name="animal handling";
    prof=false;
    modifier=0;
    corestat = "wis";
  } ::
  {
    name="athletics";
    prof=false;
    modifier=0;
    corestat = "str";
  } ::
  {
    name="deception";
    prof=false;
    modifier=0;
    corestat = "cha";
  } ::
  {
    name="history";
    prof=false;
    modifier=0;
    corestat = "int";
  } ::
  {
    name="insight";
    prof=false;
    modifier=0;
    corestat = "wis";
  } ::
  {
    name="acrobatics";
    prof=false;
    modifier=0;
    corestat = "dex";
  } ::
  {
    name="intimidation";
    prof=false;
    modifier=0;
    corestat = "cha";
  } ::
  {
    name="medicine";
    prof=false;
    modifier=0;
    corestat = "wis";
  } ::
  {
    name="investigation";
    prof=false;
    modifier=0;
    corestat = "int";
  } ::
  {
    name="nature";
    prof=false;
    modifier=0;
    corestat = "int";
  } ::
  {
    name="perception";
    prof=false;
    modifier=0;
    corestat = "wis";
  } ::
  {
    name="performance";
    prof=false;
    modifier=0;
    corestat = "cha";
  } ::
  {
    name="persuasion";
    prof=false;
    modifier=0;
    corestat = "cha";
  } ::
  {
    name="religion";
    prof=false;
    modifier=0;
    corestat = "int";
  } ::
  {
    name="sleight of hand";
    prof=false;
    modifier=0;
    corestat = "dex";
  } ::
  {
    name="stealth";
    prof=false;
    modifier=0;
    corestat = "dex";
  } ::
  {
    name="survival";
    prof=false;
    modifier=0;
    corestat = "wis";
  } ::
  []

let rec skill_set c (s:skill list) =
  match s with
  | [] -> []
  | h::t -> let skill1 = if h.corestat = "str" then
                let new_mod = h.modifier+c.str_mod in
                {h with modifier = new_mod } else
              if h.corestat = "dex" then
                let new_mod = h.modifier+c.dex_mod in
                {h with modifier = new_mod } else
              if h.corestat = "cha" then
                let new_mod = h.modifier+c.char_mod in
                {h with modifier = new_mod } else
              if h.corestat = "int" then
                let new_mod = h.modifier+c.int_mod in
                {h with modifier = new_mod } else
              if h.corestat = "wis" then
                let new_mod = h.modifier+c.wis_mod in
                {h with modifier = new_mod } else h in
    let skill2 = if skill1.prof then let newer_mod = skill1.modifier+c.prof_bonus in
        {skill1 with modifier = newer_mod} else h
in skill2::(skill_set c t)

let blank_char = {
  name = "Allan";
  race = Human;
  c_class = Barbarian;
  prof_bonus = 0;
  passive_wisdom = 0;
  armor_class = 0;
  constitution = 0;
  cons_mod = 0;
  charisma = 0;
  char_mod = 0;
  wisdom = 0;
  wis_mod = 0;
  intel = 0;
  int_mod = 0;
  strength = 0;
  str_mod = 0;
  dexterity = 0;
  dex_mod = 0;
  speed = 0;
  max_hp = 0;
  hp = 0;
  hd = 0;
  hd_qty = 0;
  xp = 0;
  level = 1;
  skills = all_skills;
  spells = [];
  equipped = [],0;
  inv = [],0;
  money = 0;
}



let quickbuild n c r =
  let cls = class_of_string c in
  let race = race_of_string r in
    let stats = stat_lister [] in
    let step1 = (*core stats*)
      match cls with
        | Barbarian -> {blank_char with
                        constitution = List.hd stats;
                        charisma = List.nth stats 1;
                        wisdom = List.nth stats 2;
                        intel = List.nth stats 3;
                        strength = List.nth stats 4;
                        dexterity = List.nth stats 5; }
        | Bard -> {blank_char with
                   constitution = List.hd stats;
                   charisma = List.nth stats 1;
                   wisdom = List.nth stats 2;
                   intel = List.nth stats 3;
                   strength = List.nth stats 4;
                   dexterity = List.nth stats 5; }
        | Cleric -> {blank_char with
                     constitution = List.hd stats;
                     charisma = List.nth stats 1;
                     wisdom = List.nth stats 2;
                     intel = List.nth stats 3;
                     strength = List.nth stats 4;
                     dexterity = List.nth stats 5; }
        | Druid -> {blank_char with
                    constitution = List.hd stats;
                    charisma = List.nth stats 1;
                    wisdom = List.nth stats 2;
                    intel = List.nth stats 3;
                    strength = List.nth stats 4;
                    dexterity = List.nth stats 5; }
        | Fighter -> {blank_char with
                      constitution = List.hd stats;
                      charisma = List.nth stats 1;
                      wisdom = List.nth stats 2;
                      intel = List.nth stats 3;
                      strength = List.nth stats 4;
                      dexterity = List.nth stats 5; }
        | Monk -> {blank_char with
                   constitution = List.hd stats;
                   charisma = List.nth stats 1;
                   wisdom = List.nth stats 2;
                   intel = List.nth stats 3;
                   strength = List.nth stats 4;
                   dexterity = List.nth stats 5; }
        | Paladin -> {blank_char with
                      constitution = List.hd stats;
                      charisma = List.nth stats 1;
                      wisdom = List.nth stats 2;
                      intel = List.nth stats 3;
                      strength = List.nth stats 4;
                      dexterity = List.nth stats 5; }
        | Ranger -> {blank_char with
                     constitution = List.hd stats;
                     charisma = List.nth stats 1;
                     wisdom = List.nth stats 2;
                     intel = List.nth stats 3;
                     strength = List.nth stats 4;
                     dexterity = List.nth stats 5; }
        | Rogue -> {blank_char with
                    constitution = List.hd stats;
                    charisma = List.nth stats 1;
                    wisdom = List.nth stats 2;
                    intel = List.nth stats 3;
                    strength = List.nth stats 4;
                    dexterity = List.nth stats 5; }
        | Sorcerer -> {blank_char with
                       constitution = List.hd stats;
                       charisma = List.nth stats 1;
                       wisdom = List.nth stats 2;
                       intel = List.nth stats 3;
                       strength = List.nth stats 4;
                       dexterity = List.nth stats 5; }
        | Warlock -> {blank_char with
                      constitution = List.hd stats;
                      charisma = List.nth stats 1;
                      wisdom = List.nth stats 2;
                      intel = List.nth stats 3;
                      strength = List.nth stats 4;
                      dexterity = List.nth stats 5; }
        | Wizard -> {blank_char with
                     constitution = List.hd stats;
                     charisma = List.nth stats 1;
                     wisdom = List.nth stats 2;
                     intel = List.nth stats 3;
                     strength = List.nth stats 4;
                     dexterity = List.nth stats 5; }
    in
    let hit = (*Database.get_hd c*) 10 in
    let step2 = (* non core stats, speed, ability modifiers*)
                {step1 with
                 name = n;
                 race = race;
                 cons_mod = ability_mod step1.constitution;
                 char_mod = ability_mod step1.charisma ;
                 wis_mod = ability_mod step1.wisdom;
                 int_mod = ability_mod step1.intel ;
                 str_mod = ability_mod step1.strength;
                 dex_mod = ability_mod step1.dexterity;
                 prof_bonus = 2;
                 hd = hit;
                 hd_qty = 1;
                 max_hp = hit;
                 hp = hit;
                 speed = (*Database.get_speed r*) 40;
                }

      in
      let step3 = {step2 with
                   spells = []; (*Database.get_spells c*)
                   skills = skill_set step2 all_skills;
                   armor_class = 10+step2.dex_mod;

                  }in (*initializing skills and items*)
    step3
