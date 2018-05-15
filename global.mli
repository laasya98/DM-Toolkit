open Str

type stat =
  | Constitution
  | Charisma
  | Wisdom
  | Intel
  | Str
  | Dex
  | HP

(* For now, just change a stat by set amount. *)
type effect = {
  stat:stat;
  die:string;
  bonus: int;
}

(* [quantity] is a type for that supports an infinite number of a thing.*)
type quantity = Int of int | Infinity

(* [wtype] is the type of a weapon, which is either Ranged or Melee.  *)
type wtype = Ranged | Melee

(* [weapontype] has a type (ranged or melee), a damage count, and a number of
   dice to roll on a hit.*)
type weapontype = {
  t : wtype;
  damage : int;
  dice : string;
}

(* [armortype] contains the armor count for a piece of armor.*)
type armortype = {
  ac : int
}

(* [itemtype] is a type of item that can be found in the game, on a character,
   or in an inventory.*)
type itemtype =
  | Weapon of weapontype
  | Shield
  | Armor of armortype
  | Ring
  | Other

(* [item] has a name, an item type, a description, a weight, an effect, a
   quantity of uses, and a value.*)
type item = {
  name:string;
  i_type:itemtype;
  description:string;
  weight:int;
  effect:effect;
  value:int;
}

type damage_spell ={
  damage_die: string;
  bonus_damage: int;
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
    duration: int;
  }

(* [role] is the role of a character.
   Party signals a player character.
   A Friendly npc will aid the characters in the event
   A Hostile npc will oppose the characters in the event
   A Neutral npc will do neither (ex: shopkeeper).
   "All" is not a role and is used for matching to all.*)
type role = All | Party | Hostile | Friendly | Neutral

(* [roll_dice] str is the value of the randomly rolled dice represented by
    string str. roll_dice accepts strings of the form "ndn" or ndntn"
    where the first n is the number of dice, the second n is the value of those
    dice, and the final n is the number of the rolled dice to "take", the rest
    to be discarded starting with the lowest roll. They can be added separating
    them by a space. e.g. "2d6t1 6d4 1d20"."*)
val roll_dice_string : string -> int

(* [roll_dice_int n d t] rolls n dice of type d and takes t of them, starting
   with the lowest roll dropped first. e.g. roll_dice 1 1 1 returns 1.  *)
val roll_dice_int : int -> int -> int -> int
