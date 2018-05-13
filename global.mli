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
  die:int list;
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
  dice : int list;
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
  uses:quantity;
  value:int;
}

type damage_spell ={
  saving_stat: string;
  damage_die: int list;
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

(* [role] is the role of a character.
   Party signals a player character.
   A Friendly npc will aid the characters in the event
   A Hostile npc will oppose the characters in the event
   A Neutral npc will do neither (ex: shopkeeper). *)
type role = Party | Hostile | Friendly | Neutral
