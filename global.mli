
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

type dwarf_sub =
 | Plain
 | Hill
 | Mountain

 type elf_sub =
  | Plain
  | High
  | Wood
  | Dark

type half_sub =
 | Plain
 | Lightfoot
 | Stout

type dragon_descent =
  | Black
  | Blue
  | Brass
  | Bronze
  | Copper
  | Gold
  | Green
  | Red
  | Silver
  | White

type gnome_sub =
 | Plain
 | Forest
 | Rock

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
