
(* For now, just change a stat by set amount. *)
type effect = {
  stat:string; (*TODO: temp. Coordinate with character?*)
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

(* Example:
   {
    name="Magic Missile";
    level = 1;
    saving_stat = "none";
    damage_die = [4];
    bonus_damage = 1;
    range = 120;
    targets = 3;
    multiple = true;
   }
*)

type spelltype =
  | Damage of damage_spell
  | Conjuration
  | Transmutation

type spell =
  {
    name:string;
    level:int;
    targets: int;
    to_cast: int;
    duration: int;
  }
