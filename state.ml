open Database
open Event
open Character
open Command
open Global

(** A [State] is a representation of the state of the toolkit, including every
    current location and entity, and any statuses or active effects for the
    current gamespace.*)
module type State = sig
  module D : Database
  module C : Character
  module E : Event
  module Com :Command

  type data = D.data
  type character = C.c
  type event = E.t
  type location =

  type state

  type entity =
    |Item of item
    |Character of character
    |Effect of (entity * int)
    |Event of event

  type exits = {}

  type location = {
    name : string;
    description : string;
    contents : entity list;
    exits : ( string * location ) list (*(direction, location)*)
    }

  type gamestate = {
    locations : location list;
    party : character list;
    active_events : event list;
  }

  val init_state : D.data -> state
  val current_room : state -> string
  val current_gamestate : state -> string
  val current_room_characters : state -> string list
  val rooms : state -> string list
  val effects : state -> string list
  val event : state -> event list
  val action : Com.command -> state -> state
end

module MakeState
    = functor (C:Character) -> functor (D:Database) -> functor (E:Event) ->
      functor (Com:Command) ->
    struct

    type data = D.data
    type character = C.c
    type event = E.t

(*************************** KERRI STUFF BELOW *****************************)

  (** SHOP **)
let buy_item name evt =
  match List.find_opt (fun x -> x.name =name) (E.get_items evt) with
  | Some i -> failwith "unimplemented"
  | None -> failwith "unimplemented"

  (** COMBAT **)
  (*TODO: get the EQUIPPED weapon. *)
let get_weapon c =
  let weapon =List.find (fun x ->
      match x.i_type with | Weapon _ -> true |_-> false) (C.inv c) in
  match weapon.i_type with
  | Weapon w -> w
  | _ -> failwith "No weapon"

(*TODO: get the EQUIPPED armor. *)
let get_armor c =
  let armor = List.find
      (fun x -> match x.i_type with | Armor _ -> true | _-> false) (C.inv c) in
  match armor.i_type with
  | Armor a -> a
  | _ -> failwith "No armor" (*TODO: AC for no armor? *)

(*TODO: use Dex for ranged?
  Proficiency?
  make AC
*)
let attack_roll attacker target =
  let d20 = 1+ Random.int 19 in
  let ability =
    try
      if (get_weapon attacker).t = Ranged then C.dex attacker
      else C.strength attacker
    with _ -> C.strength attacker
  in
  let prof = 0 in
  let ac = try (get_armor target).ac with _ -> 0 in
  if d20 = 1 then 0
  else if d20 = 20 then 2
  else
    let roll = d20 + ability + prof in
    if roll > ac then 1 else 0

let rec roll_dice dice acc =
  match dice with
  | [] -> acc
  | h::t -> roll_dice t (acc + 1 + Random.int h)

let damage_roll attacker crit =
  let weapon = get_weapon attacker in
  let dice = try weapon.dice with _ -> [] in
  let bonus = try weapon.damage with _ -> 0  in
  let ability = C.strength attacker in
  bonus + ability + roll_dice dice 0

let deal_damage amount target = failwith "unimplemented"

(* returns the new state of the target after being hit*)
let attack attacker target =
  let hit = attack_roll attacker target in
  if hit = 0 then target else
    deal_damage (damage_roll attacker (hit=2)) target

(*************************** KERRI STUFF ABOVE *****************************)

end
