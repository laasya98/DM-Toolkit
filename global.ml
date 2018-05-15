type stat =
  | Constitution
  | Charisma
  | Wisdom
  | Intel
  | Str
  | Dex
  | HP


type effect = {
  stat:stat;
  die:string;
  bonus: int;
}

type quantity = Int of int | Infinity

type wtype = Ranged | Melee

type weapontype = {
  t : wtype;
  damage : int;
  dice : string;
}

type armortype = {
  ac : int
}

type itemtype =
  | Weapon of weapontype
  | Shield
  | Armor of armortype
  | Ring
  | Other

type item = {
  name:string;
  i_type:itemtype;
  description:string;
  weight:int;
  effect:effect;
  value:int;
}

type damage_spell ={
  saving_stat: string;
  damage_die: string;
  bonus_damage: int;
  range: int;
  multiple : bool;
}

type spelltype =
  | Damage of damage_spell
  | Conjuration
  | Status of effect

type spell =
  {
    name:string;
    stype: spelltype;
    level:int;
    targets: int;
    to_cast: int;
    duration: int;
  }

type role = All | Party | Hostile | Friendly | Neutral


let rec dice_roller n d accum=
  if n > 0 then let one = ((Random.int (d)) + 1) in
    dice_roller (n-1) d (one :: accum)
  else accum

let rec sum_to_n n accum ls =
  if n > 0 then match ls with
    |h :: t -> sum_to_n (n-1) (accum + h) t
    |[] -> accum
  else accum

exception Bad_dice of string

let rec dice_helper str accum =
try
  let r = Str.regexp  "\\([0-9]+\\)[d]\\([0-9]*\\)[t]?\\([0-9]*\\)\\(.*\\)" in
  let n1 = int_of_string (Str.replace_first r "\\1" str) in
  let n2 = int_of_string (Str.replace_first r "\\2" str) in
  let n3 = (Str.replace_first r "\\3" str) in
  let rest = String.trim (Str.replace_first r "\\4" str) in
  let lst = (dice_roller n1 n2 []) in
  let lst' = List.sort compare lst in
  let lst'' = List.rev lst' in
  let total = if (n3 <> "") then sum_to_n (int_of_string n3) 0 lst''
    else sum_to_n (List.length lst'') 0 lst'' in
  if rest <> "" then dice_helper rest (accum + total)
  else accum + total
with _ ->  raise (Bad_dice "cannot read dice input")

let roll_dice_string str =
  try
    let str' = str|>String.trim|>String.lowercase_ascii in
    dice_helper str' 0
  with _-> -1

let roll_dice_int n d t =
  (dice_roller n d []) |> List.sort compare |> List.rev |> sum_to_n t 0
