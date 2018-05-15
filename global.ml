
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
}

type quantity = Int of int | Infinity

type wtype = Ranged | Melee

type weapontype = {
  t : wtype;
  damage : int;
  dice : string;
}

type itemtype =
  | Weapon of weapontype
  | Armor of int
  | Other

type item = {
  name:string;
  i_type:itemtype;
  weight:int;
  effect:effect;
  value:int;
}

let find_assoc n lst =
  List.find (fun (x,_) -> x=n) lst
  |> snd

let parse_effect dlist =
  try
    let s = match find_assoc "Stat" dlist with
      |"constitution" -> Constitution
      |"charisma" -> Charisma
      |"wisdom" -> Wisdom
      |"intelligence" -> Intel
      |"strength" -> Str
      |"dexterity" -> Dex
      |"hp" -> HP
      |_ -> raise (Failure "Invalid Stat")
    in
    let d = find_assoc "EDice" dlist in
    {stat=s; die=d}
  with _ -> raise (Failure "Invalid Effect Data")

let parse_weapon dlist =
  try
    let t = match find_assoc "Wtype" dlist with
      |"Ranged" -> Ranged
      |_->Melee
    in
    let dam = int_of_string (find_assoc "Damage" dlist) in
    let dice = find_assoc "WDice" dlist in
    {t=t; damage=dam; dice=dice}
  with _ -> raise (Failure "Invalid Weapon Data")

let parse_item dlist =
  try
    let n = find_assoc "Name" dlist in
    let t = find_assoc "Type" dlist in
    let t' = match t with
      |"Weapon" -> Weapon (parse_weapon dlist)
      |"Armor" -> Armor (int_of_string (find_assoc "AC" dlist))
      |_ -> Other
    in
    let w = int_of_string (find_assoc "Weight" dlist) in
    let e = parse_effect dlist in
    let v = int_of_string (find_assoc "Value" dlist) in
    {name=n; i_type=t'; weight=w;effect=e;value=v}
  with _ -> raise (Failure "Invalid Item Data")

type damage_spell ={
  damage_die: string;
  range: int;
  multiple: bool;
}

type spelltype =
  | Damage of damage_spell
  | Status of effect

type spell =
  {
    name:string;
    stype: spelltype;
    level:int;
    targets: int;
    to_cast: int;
  }

let parse_damage dlist =
  try
    let d = find_assoc "WDie" dlist in
    let r = int_of_string (find_assoc "Range" dlist) in
    let m =
      match find_assoc "Multiple" dlist with
      |"true" -> true
      |_ -> false
    in
    {damage_die = d; range = r; multiple=m}
  with _ -> raise (Failure "Invalid Damage Spell Data")

let parse_spell dlist =
  try
    let n = find_assoc "Name" dlist in
    let t = find_assoc "Type" dlist in
    let t' = match t with
      |"Damage" -> Damage (parse_damage dlist)
      |"Status" -> Status (parse_effect dlist)
      |_ -> raise (Failure "Invalid Spell Data")
    in
    let l = int_of_string (find_assoc "Level" dlist) in
    let tar = int_of_string (find_assoc "Targets" dlist) in
    let wait = int_of_string (find_assoc "Wait" dlist) in
    {name=n; stype=t'; level=l; targets=tar; to_cast=wait}
  with _ -> raise (Failure "Invalid Spell Data")

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
