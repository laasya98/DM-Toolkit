open Global

module type Character = sig

  type skill
  type ability

  (* type c_class is the types of classes of a character*)
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

  (* type c_class is the types of races of a character*)
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

    armor_class: int ;

    constitution: int;
    charisma: int;
    wisdom:int;
    intel:int;
    strength:int;
    dexterity:int;

    speed:int;
    max_hp:int;
    hp:int;

    hd:int;
    hd_qty:int;

    xp:int;
    level:int;

    skills: skill list;
    abilities: ability list;
    equipped: ((item * int) list )* int;
    inv: ((item * int) list )* int;
    money: int;
  }

  val name :  c -> string
  val wisdom :  c -> int
  val update_wisdom :  c -> int ->  c
  val armor_class :  c -> int
  val update_ac :  c -> int -> c
  val dex :  c -> int
  val update_dex :  c -> int -> c
  val intel :  c -> int
  val update_intel :  c -> int -> c
  val strength :  c -> int
  val update_strength :  c -> int -> c
  val speed :  c -> int
  val update_speed :  c -> int -> c
  val curr_hp :  c -> int
  val update_hp :  c -> int -> c
  val max_hp :  c -> int
  val update_max_hp :  c -> int -> c
  val xp :  c -> int
  val update_xp :  c -> int -> c
  val level :  c -> int
  val level_up :  c -> int -> c
  val skills :  c -> skill list
  val add_skill :  c -> skill -> c
  val abilities :  c -> ability list
  val add_ability :  c -> ability -> c
  val inv :  c -> (item*int) list
  val add_item :  c -> item -> int -> c
  val equipped :  c -> (item*int) list
  val equip :  c -> item -> int -> c
  val money :  c -> int
  val update_money :  c -> int -> c
  val const :  c -> int
  val update_const :  c -> int -> c
  val charisma :  c -> int
  val update_charisma :  c -> int -> c

end

module Character = struct
  type item = Global.item
  type skill
  type ability

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

    armor_class: int ;

    constitution: int;
    charisma: int;
    wisdom:int;
    intel:int;
    strength:int;
    dexterity:int;

    speed:int;
    max_hp:int;
    hp:int;

    hd:int;
    hd_qty:int;

    xp:int;
    level:int;

    skills: skill list;
    abilities: ability list;
    equipped: ((item * int) list )* int;
    inv: ((item * int) list )* int;
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
  let add_skill c s =
    let skills = c.skills in
    {c with skills = s::skills}
  let abilities c = c.abilities
  let add_ability c a =
    let abilities = c.abilities in
    {c with abilities = a::abilities}
  let inv c : (item*int) list = fst c.inv
  let equipped c : (item*int) list= fst c.equipped

  let insert_qty i n l =
    if List.mem_assoc i l then
      List.map(fun (a,b) -> if i = a then (i,b+n) else (a,b)) l else
      (i,n)::l
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

  let quickbuild c r =
    {
      name = "Allan";
      race = r;
      c_class = c;
      armor_class = 0;
      constitution = 0;
      charisma = 0;
      wisdom = 0;
      intel = 0;
      strength = 0;
      dexterity = 0;
      speed = 0;
      max_hp = 0;
      hp = 0;
      hd = 0;
      hd_qty = 0;
      xp = 0;
      level = 0;
      skills = [];
      abilities = [];
      equipped = [],0;
      inv = [],0;
      money = 0;
  }

end
