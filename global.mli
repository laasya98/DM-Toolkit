
(* For now, just change a stat by set amount. *)
type effect = {
  stat:string; (*TODO: temp. Coordinate with character?*)
  amount:int;
}

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
  uses:int;
  value:int;
}
