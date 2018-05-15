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

let ability_mod a =
  let b = a - 10 in
  if (b/2)*2 = b
  then b/2 else (b-1)/2

  let name  c = c.name
  let race  c = c.race
  let class_of  c = c.c_class
  let wisdom  c = c.wis_mod
let update_wisdom  c w = {c with wisdom = w;
                         wis_mod = ability_mod w}
  let const  c = c.cons_mod
  let update_const  c o = {c with constitution = o;
                        cons_mod = ability_mod o}
  let armor_class  c = c.armor_class
  let update_ac c a = {c with armor_class = a;}
  let dex  c = c.dex_mod
  let update_dex c d = {c with dexterity = d;
                     dex_mod = ability_mod d}
  let intel c = c.int_mod
  let update_intel c i = {c with intel = i;
                       int_mod = ability_mod i}
  let strength c = c.str_mod
  let update_strength c s = {c with strength = s;
                          str_mod = ability_mod s}
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
(*TODO:
  - add hd to hp
  - new hd after every 5 levels
  - prof bonus increase
  - ability score increase
  - reset skills *)
    }
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
    let remove_item c i n = c (*TODO: THIS. BC ITS UNIMPLEMENTED*)
    let money c = c.money
    let update_money c m = {c with money = m}
    let charisma c = c.char_mod
    let update_charisma c a = {c with charisma = a;
                            char_mod = ability_mod a}


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

let string_of_class c =
  match c with
  | Barbarian -> "Barbarian"
  | Bard -> "Bard"
  | Cleric -> "Cleric"
  | Druid -> "Druid"
  | Fighter -> "Fighter"
  | Monk-> "Monk"
  | Paladin -> "Paladin"
  | Ranger -> "Ranger"
  | Rogue -> "Rogue"
  | Sorcerer -> "Sorcerer"
  | Warlock -> "Warlock"
  | Wizard -> "Wizard"

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
let string_of_race r =
  match r with
  | Dwarf -> "Dwarf"
  | Elf -> "Elf"
  | Halfling -> "Halfling"
  | Human -> "Human"
  | Dragonborn -> "Dragonborn"
  | Gnome -> "Gnome"
  | Half_Elf -> "Half-Elf"
  | Half_Orc -> "Half-Orc"
  | Tiefling -> "Tiefling"


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



  let rec stat_lister acc : int list=
    if List.length acc = 6
      then
        acc |> List.sort compare |> List.rev
      else
        let stat =  Global.roll_dice_int 4 6 3 in
        stat_lister (stat::acc)

let rec skill_set (s:skill list) c  =
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
        {skill1 with modifier = newer_mod} else skill1
in skill2::(skill_set t c)


let parse_char clist =
  try
    let n = find_assoc "Name" clist in
    let cls = (find_assoc "Class" clist) |> class_of_string in
    let r = (find_assoc "Race" clist) |> race_of_string in
    let ac = (find_assoc "Armor Class" clist) |> int_of_string in
    let pw = (find_assoc "Passive Wisdom" clist) |> int_of_string  in
    let pb = (find_assoc "Prof Bonus" clist) |> int_of_string  in
    let co = (find_assoc "Constitution" clist) |> int_of_string  in
    let com = (find_assoc "Cons Mod" clist) |> int_of_string  in
    let ch = (find_assoc "Charisma" clist) |> int_of_string  in
    let chm = (find_assoc "Char Mod" clist) |> int_of_string  in
    let dex = (find_assoc "Dexterity" clist) |> int_of_string  in
    let dm = (find_assoc "Dex Mod" clist) |> int_of_string  in
    let wis = (find_assoc "Wisdom" clist) |> int_of_string  in
    let wm = (find_assoc "Wis Mod" clist) |> int_of_string  in
    let intl = (find_assoc "Intelligence" clist) |> int_of_string  in
    let im = (find_assoc "Int Mod" clist) |> int_of_string  in
    let st = (find_assoc "Strength" clist) |> int_of_string  in
    let sm = (find_assoc "Str Mod" clist) |> int_of_string  in
    let sp = (find_assoc "Speed" clist) |> int_of_string  in
    let mhp = (find_assoc "Max HP" clist) |> int_of_string  in
    let hp = (find_assoc "HP" clist) |> int_of_string  in
    let hd = (find_assoc "HD" clist) |> int_of_string  in
    let hdq = (find_assoc "HD qty" clist) |> int_of_string  in
    let xp = (find_assoc "XP" clist) |> int_of_string  in
    let lvl = (find_assoc "Level" clist) |> int_of_string  in
    let cash = (find_assoc "Money" clist) |> int_of_string  in

    (*let t' = match t with
      |"Weapon" -> Weapon (parse_weapon dlist)
      |"Armor" -> Armor (int_of_string (find_assoc "AC" dlist))
      |_ -> Other
      in*)

    { name=n;
      race=r;
      c_class=cls;
      armor_class=ac;
      prof_bonus=pb;
      passive_wisdom=pw;
      constitution = co;
      cons_mod=com;
      charisma=ch;
      char_mod=chm;
      wisdom=wis;
      wis_mod=wm;
      intel=intl;
      int_mod=im;
      strength=st;
      str_mod=sm;
      dexterity=dex;
      dex_mod=dm;
      speed=sp;
      max_hp=mhp;
      hp=hp;
      hd=hd;
      hd_qty=hdq;
      xp=xp;
      level=lvl;
      skills= all_skills;
      spells= [];
      equipped= [],0;
      inv= [],0;
      money=cash;
    } |> skill_set all_skills
  with _ -> raise (Failure "Invalid Character Data")

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
let remove s lst = (List.filter (fun x -> x<>s) lst)
let stat_list = ["strength";"charisma";"dexterity";"intelligence";"constitution";"wisdom"]
let rec assign_stats s l c =
  match s with
  | [] -> c
  | h::t -> begin
      match String.lowercase_ascii h with
      | "strength" -> assign_stats (t) (List.tl l) {c with strength = List.hd l}
      | "dexterity" -> assign_stats (t) (List.tl l) {c with dexterity = List.hd l}
      | "charisma" -> assign_stats (t) (List.tl l) {c with charisma = List.hd l}
      | "constitution" -> assign_stats (t) (List.tl l) {c with constitution = List.hd l}
      | "intelligence" -> assign_stats (t) (List.tl l) {c with intel = List.hd l}
      | "wisdom" -> assign_stats (t) (List.tl l) {c with wisdom = List.hd l}
      | _ -> c
    end

let quickbuild n c r =
    let cls = class_of_string c in
    let race = race_of_string r in
    let stats = stat_lister [] in
    let prim_stats = Database.primary_stat c in
    let all_stats = stat_list in

    let step1a = (*core stats*)
      let first = (fst prim_stats) |> String.lowercase_ascii in
      match first with
      | "strength" -> {blank_char with strength = List.hd stats}, (remove "strength" all_stats)
      | "dexterity" -> {blank_char with dexterity = List.hd stats}, (remove "dexterity" all_stats)
      | "charisma" -> {blank_char with charisma = List.hd stats}, (remove "charisma" all_stats)
      | "constitution" -> {blank_char with constitution = List.hd stats}, (remove "constitution" all_stats)
      | "intelligence" -> {blank_char with intel = List.hd stats}, (remove "intelligence" all_stats)
      | "wisdom" -> {blank_char with wisdom = List.hd stats}, (remove "wisdom" all_stats)
      | _ -> {blank_char with strength = List.hd stats}, (remove "strength" all_stats)
    in
    let step1b =
      let second = (snd prim_stats) |> String.lowercase_ascii in
      let temp = fst step1a in
      let al_stats = snd step1a in
      match second with
      | "strength" -> {temp with strength = List.nth stats 2}, (remove "strength" al_stats)
      | "dexterity" -> {temp with dexterity = List.nth stats 2}, (remove "dexterity" al_stats)
      | "charisma" -> {temp with charisma = List.nth stats 2}, (remove "charisma" al_stats)
      | "constitution" -> {temp with constitution = List.nth stats 2}, (remove "constitution" al_stats)
      | "intelligence" -> {temp with intel = List.nth stats 2}, (remove "intelligence" al_stats)
      | "wisdom" -> {temp with wisdom = List.nth stats 2}, (remove "wisdom" al_stats)
      | _ -> if temp.strength <> 0
        then {temp with charisma = List.nth stats 2}, (remove "charisma" al_stats)
        else {temp with strength = List.nth stats 2}, (remove "strength" al_stats)
    in
    let new_stats = stats |> List.tl |> List.tl in
    let step1 = assign_stats (snd step1b) new_stats (fst step1b) in

    let hit = Database.hit_die c in
    let step2 = (* non core stats, speed, ability modifiers*)
                {step1 with
                 name = n;
                 race = race;
                 c_class = cls;
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
                 speed = (*Database.speed_of r*) 40;
                }

      in
      let step3 =  (*initializing skills and items*)
              {step2 with
                   spells = []; (*Database.get_spells c*)
                   skills = skill_set all_skills step2;
                   passive_wisdom = 10 + step2.wis_mod + step2.prof_bonus;
                   armor_class = 10+step2.dex_mod;
                  }
      in
      step3

let details c =
  "Character " ^ c.name ^ ":\n" ^
    "Class: " ^ (c.c_class |> string_of_class) ^ "\n" ^
    "Race: " ^ (c.race|> string_of_race)  ^ "\n" ^
    "HP: " ^ (c.hp |> string_of_int)  ^ "\n" ^
    "Strength: " ^ (c.strength |> string_of_int)  ^ "\n" ^
    "Wisdom: " ^ (c.wisdom |> string_of_int)  ^ "\n" ^
    "Intelligence: " ^ (c.intel |> string_of_int) ^  "\n" ^
    "Dexterity: " ^ (c.dexterity |> string_of_int)  ^ "\n" ^
    "Constitution: " ^ (c.constitution |> string_of_int)  ^ "\n" ^
    "Charisma: " ^ (c.charisma |> string_of_int)  ^ "\n"
