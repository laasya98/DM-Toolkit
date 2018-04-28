open Global

module type Character = sig

  type skill
  type ability
  type c_class

  type c = {
        name:string;
        status:string;
        defense: int ;
        intel:int;
        strength:int;
        dexterity:int;
        speed:int;
        max_hp:int;
        hp:int;
        xp:int;
        level:int;
        skills: skill list;
        abilities: ability list;
        equipped: item list;
        inv: item list;
      }

  val name :  c -> string
  val status :  c -> string
  val update_status :  c -> c
  val defense :  c -> int
  val update_def :  c -> int -> c
  val dex :  c -> int
  val update_dex :  c -> int -> c
  val intel :  c -> int
  val update_intel :  c -> int -> c
  val strength :  c -> int
  val update_strength :  c -> int -> c
  val speed :  c -> int
  val update_speed :  c -> int -> c
  val curr_hp :  c -> int
  val update_hp :  c -> int -> c
  val max_hp :  c -> int
  val xp :  c -> int
  val update_xp :  c -> int -> c
  val level :  c -> int
  val level_up :  c -> int -> c
  val skills :  c -> skill list
  val add_skill :  c -> skill -> c
  val abilities :  c -> ability list
  val add_ability :  c -> ability -> c
  val inv :  c -> item list
  val add_item :  c -> item -> c
  val equipped :  c -> item list
  val equip :  c -> item -> c

end

module Character = struct
  type item = Global.item (*TODO fix i guess*)
  type skill
  type ability
  type c_class

  type c = {
    name:string;
    status:string;
    defense: int ;
    intel:int;
    strength:int;
    dexterity:int;
    speed:int;
    max_hp:int;
    hp:int;
    xp:int;
    level:int;
    skills: skill list;
    abilities: ability list;
    equipped: item list;
    inv: item list;
  }

  let name  c = c.name
  let status c = c.status
  let update_status c = c (*TODO what*)
  let defense  c = c.defense
  let update_def c d = {c with defense = d;}
  let dex  c = c.dexterity
  let update_dex c d = {c with dexterity = d;}
  let intel c = c.intel
  let update_intel c i = {c with intel = i;}
  let strength c = c.strength
  let update_strength c s = {c with strength = s}
  let speed c = c.speed
  let update_speed c s = {c with speed = s}
  let curr_hp c = c.hp
  let update_hp c h = {c with hp = h}
  let max_hp c = c.max_hp
  let xp c = c.xp
  let update_xp c x = {c with xp = x}
  let level  c = c.level
  let level_up  c l =
    {c with
      level = l
    }
  (* TODO: whatever algorithm updates strength/skills/speed based off of level/chartype*)
  let skills c =  c.skills
  let add_skill c s =
    let skills = c.skills in
    {c with skills = s::skills}
  let abilities c = c.abilities
  let add_ability c a =
    let abilities = c.abilities in
    {c with abilities = a::abilities}
  let inv c = c.inv
  let add_inv c i =
    let items = c.inv in
    {c with inv = i::items}
  let equipped c = c.equipped
  let equip c e =
    let equipment = c.equipped in
    if List.mem e c.inv then
      {c with equipped = e::equipment}
    else c

end
