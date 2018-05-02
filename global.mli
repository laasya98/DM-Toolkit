

(* For now, just change a stat by set amount. *)
type effect = {
  stat:string; (*TODO: temp. Coordinate with character.*)
  amount:int;
}

type quantity = Int of int | Infinity

type wtype = Ranged | Melee

type weapontype = {
  t : wtype;
  damage : int;
  dice : int list;
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
  | Transmutation

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
