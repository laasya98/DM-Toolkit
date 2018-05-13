open Global

  (* type skill should be all the possible skill types a player can potentially have
     it stores an int as the modifier and a bool as whether or not the skill is proficient*)

  type skill= {
    name:string;
    prof:bool;
    modifier:int;
    corestat:string;
    }
 (*type spell is a global*)
  type spell = Global.spell

  (* not thinking abt subclasses right now
 type elf_sub =
    | High
    | Wood
    | Dark

  type half_sub =
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
   | Forest
   | Rock*)



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


  (* type t is the character type. this should contain a record of information
     about a character in the game. this record can be updated by state and events *)
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
    equipped: ((item * int) list )* int;
    inv: ((item * int) list )* int;
    money: int;
  }

  (* [name character] is a string containing the character's title. *)
  val name :  c -> string

  (* [wisdom character] is an int that describes the player's wisdom stat. *)
  val wisdom :  c -> int

  (* [update_wisdom character new_w ] character with wisdom = new_w. *)
  val update_wisdom :  c -> int ->  c

  (* [armor_class character] is an int that describes the player's armorclass stat. *)
  val armor_class :  c -> int

  (* [update_def character new_ac] character with ac = new_ac. *)
  val update_ac :  c -> int -> c

  (* [dex character] is an int that describes the player's dexterity stat. *)
  val dex :  c -> int

  (* [update_dex character new_d] character with dex = new_d. *)
  val update_dex :  c -> int -> c

  (* [intel character] is an int that describes the player's intelligence stat. *)
  val intel :  c -> int

  (* [update_intel character new_i] character with intel = new_i. *)
  val update_intel :  c -> int -> c

  (* [strength character] is an int that describes the player's strength stat. *)
  val strength :  c -> int

  (* [update_strength character new_s] character with strength = new_s. *)
  val update_strength :  c -> int -> c

  (* [speed character] is an int that describes the player's speed stat. *)
  val speed :  c -> int

  (* [update_speed character new_s] character with speed = new_s. *)
  val update_speed :  c -> int -> c

  (* [curr_hp character] is an int that contains the player's current hit points.*)
  val curr_hp :  c -> int

  (* [update_hp character new_hp] character with hp = new_hp.*)
  val update_hp :  c -> int -> c

  (* [max_hp character] is an int that contains the player's max possible hit points.*)
  val max_hp :  c -> int

  (* [update_max_hp character new_hp] character with max_hp = new_hp.*)
  val update_max_hp :  c -> int -> c

  (* [xp character] is an int that contains the player's experience points . *)
  val xp :  c -> int

  (* [update_xp character new_xp] character with xp = new_xp. *)
  val update_xp :  c -> int -> c

  (* [level character] is an int that describes the character's current level . *)
  val level :  c -> int

  (* [level_up character lvl] increments character's level by lvl *)
  val level_up :  c -> int -> c

  (* [skills character] is a list of skills a player has. *)
  val skills :  c -> skill list

  (* [spells character] all the spells a character can perform *)
  val spells :  c -> spell list

  (* [add_spell character spell] adds spell to the spells a character can perform. *)
  val add_spell :  c -> spell -> c

  (* [inv character] is a list of the items a player has. *)
  val inv :  c -> (item*int) list

  (* [add_item character i] adds i to the characters inventory.*)
  val add_item :  c -> item -> int -> c

  (* [equipped character] is a list of the items a player has equipped. *)
  val equipped :  c ->  (item*int) list

  (* [equip character e] adds e to the characters equpped items.
     Requires that e is present in the character's inventory*)
  val equip :  c -> item -> int -> c

  (*[money character] is an int describing how much money a character has *)
  val money :  c -> int

  (* [update_money character cash] character with money = cash *)
  val update_money :  c -> int -> c

  (* [const character] is an int that describes the player's constitution stat. *)
  val const :  c -> int

  (* [update_const character new_c] character with constitution = new_c.*)
  val update_const :  c -> int -> c

  (* [charisma character] is an int that describes the player's charisma stat. *)
  val charisma :  c -> int

  (* [update_charisma character new_c] character with charisma = new_c.*)
  val update_charisma :  c -> int -> c
