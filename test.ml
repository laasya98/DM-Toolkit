open OUnit2
open Event
open Character
open Global
open State

let item1 = {
  name="item1";
  i_type=Other;
  description = "item1 desc";
  weight = 1;
  effect = {stat=""; amount=0};
  uses = Int 0;
  value=10;
}

let item2 = {
  name="item2";
  i_type=Shield;
  description = "item2 desc";
  weight = 1;
  effect = {stat=""; amount=0};
  uses = Int 0;
  value=10;
}

let char1:Character.c = {
  name="char1";
  status="";
  race = Human;
  c_class = Barbarian;
  defense=0;
  wisdom=0;
  charisma=0;
  constitution = 0;
  money = 10;
  intel=0;
  strength=2;
  dexterity=1;
  speed=1;
  max_hp=10;
  hp=10;
  xp=0;
  level=1;
  skills=[];
  abilities=[];
  equipped=[],3;
  inv=[item2,1],3;
}

let char2:Character.c = {
  name="char2";
  status="";
  race = Halfling;
  c_class = Sorcerer;
  wisdom=0;
  charisma = 0;
  constitution = 0;
  money = 10;
  defense=0;
  intel=0;
  strength=2;
  dexterity=1;
  speed=1;
  max_hp=10;
  hp=5;
  xp=0;
  level=1;
  skills=[];
  abilities=[];
  equipped=[],3;
  inv=[item1,2],3;
}

let character_tests = [
  "name" >:: (fun _ -> assert_equal "char2" (Character.name char2));
  "wisdom" >:: (fun _ -> assert_equal 0 (Character.wisdom char1));
  "update wisdom" >:: (fun _ -> assert_equal 10
                          (Character. wisdom (Character.update_wisdom char1 10)));
  "defense" >:: (fun _ -> assert_equal 0 (Character.defense char1));
  "update defense" >:: (fun _ -> assert_equal 10
                          (Character.defense(Character.update_def char1 10)));
  "speed" >:: (fun _ -> assert_equal 1 (Character.speed char2));
  "update speed" >:: (fun _ -> assert_equal 10
                          (Character.speed (Character.update_speed char1 10)));
  "strength" >:: (fun _ -> assert_equal 2 (Character.strength char1));
  "update strength" >:: (fun _ -> assert_equal 10
                          (Character.strength (Character.update_strength char1 10)));
  "intel" >:: (fun _ -> assert_equal 0 (Character.intel char1));
  "update intel" >:: (fun _ -> assert_equal 10
                         (Character.intel (Character.update_intel char2 10)));
  "dex" >:: (fun _ -> assert_equal 1 (Character.dex char1));
  "update dex" >:: (fun _ -> assert_equal 10
                       (Character.dex (Character.update_dex char1 10)));
  "hp" >:: (fun _ -> assert_equal 10 (Character.curr_hp char1));
  "update hp" >:: (fun _ -> assert_equal 8
                      (Character.curr_hp (Character.update_hp char1 8)));
  "update hp bad" >:: (fun _ -> assert_equal 10
                      (Character.curr_hp (Character.update_hp char1 50)));
  "max hp" >:: (fun _ -> assert_equal 10 (Character.max_hp char1));
  "update max hp" >:: (fun _ -> assert_equal 50
                      (Character.max_hp (Character.update_max_hp char1 50)));
  "update hp not bad" >:: (fun _ -> assert_equal 50
                      (Character.curr_hp (Character.update_hp
                      (Character.update_max_hp char1 50) 50)));
  "xp" >:: (fun _ -> assert_equal 0 (Character.xp char1));
  "update xp" >:: (fun _ -> assert_equal 800
                      (Character.xp (Character.update_xp char1 800)));
  "inv" >:: (fun _ -> assert_equal [(item2,1)] (Character.inv char1));
  "add item" >:: (fun _ -> assert_equal [item1,1;item2,1]
                     (Character.inv (Character.add_item char1 item1 1)));
  "equipped stuff" >:: (fun _ -> assert_equal [] (Character.equipped char1));
  "good equip" >:: (fun _ -> assert_equal [item2,1]
                       (Character.equipped (Character.equip char1 item2 1)));
  "bad equip" >:: (fun _ -> assert_equal []
                      (Character.equipped (Character.equip char1 item1 1)));
]

let evtC1 = Event.make_event "evtC1" Battle [] ["A";"B";"C"]
let evtC2 = Event.make_event "evtC1" Battle [(item1,Int 1)] []
let evtS = Event.make_event "evtS" Shop [(item1,Int 3); (item2, Infinity)] []

let event_tests = [
  "form" >:: (fun _ -> assert_equal (Battle:Event.form) (Event.get_form evtC1));
  "name" >:: (fun _ -> assert_equal "evtC1" (Event.get_name evtC1));
  "get_items" >:: (fun _ -> assert_equal [] (Event.get_items evtC1));
  "get_turn" >:: (fun _ -> assert_equal 0 (Event.get_turn evtC1));
  "update_turn" >:: (fun _ -> assert_equal 1 (Event.turn evtC1 |> Event.get_turn));
  "get_tlst" >:: (fun _ -> assert_equal ["A";"B";"C"] (Event.get_turnlst evtC1));
  "update_tlst" >:: (fun _ -> assert_equal ["B";"C";"A"] (Event.turn evtC1 |> Event.get_turnlst));
  "add_item1" >:: (fun _ -> assert_equal [(item1,Int 1)] (Event.add_item item1 (Int 1) evtC1 |> Event.get_items));
  "add_item2" >:: (fun _ -> assert_equal [(item1,Int 5)] (Event.add_item item1 (Int 4) evtC2 |> Event.get_items));
  "add_item3" >:: (fun _ -> assert_equal [(item1,Infinity)] (Event.add_item item1 (Infinity) evtC1 |> Event.get_items));
  "add_item3" >:: (fun _ -> assert_equal [(item1,Infinity)] (Event.add_item item1 (Infinity) evtC2 |> Event.get_items));
  "remove_item1" >:: (fun _ -> assert_equal [] (Event.remove_item "item1" (Int 1) evtC1 |> Event.get_items));
  "remove_item2" >:: (fun _ -> assert_equal [] (Event.remove_item "item1" (Int 1) evtC2 |> Event.get_items));
  "remove_item3" >:: (fun _ -> assert_equal [] (Event.remove_item "item1" (Infinity) evtC2 |> Event.get_items));
  "remove_item4" >:: (fun _ -> assert_equal [] (Event.remove_item "item1" (Infinity) evtC2 |> Event.get_items));
  "remove_item5" >:: (fun _ -> assert_equal [(item1,Int 3)] (Event.remove_item "item2" (Infinity) evtS |> Event.get_items));
  "remove_item6" >:: (fun _ -> assert_equal [(item1,Int 3); (item2, Infinity)] (Event.remove_item "item2" (Int 4) evtS |> Event.get_items));
  "remove_item6" >:: (fun _ -> assert_equal [(item1,Int 1); (item2, Infinity)] (Event.remove_item "item1" (Int 2) evtS |> Event.get_items));

  "changeF" >:: (fun _ -> assert_equal (Interaction:Event.form) (Event.change_form Interaction evtC1 |> Event.get_form));
]

type state = State.state

let loc:(State.location) = {name="loc1"; description=""; characters=[]; contents=[]; exits =[]}
let st1:(state) = {locations=[]; characters=[(char1, Party); (char2,Hostile)]; event=evtC2; current_location=loc; output=""}
let st2:(state) = {locations=[]; characters=[(char1, Party)]; event=evtS;current_location=loc; output=""}

let state_tests = [
  (*Combat*)
  "comb invA" >:: (fun _ -> assert_equal "Action Failed: Invalid Attacker Name" (State.action (Fight ("nop","char2")) st1 |> State.output));
  "comb invT" >:: (fun _ -> assert_equal "Action Failed: Invalid Target Name" (State.action (Fight ("char1","nop")) st1 |> State.output));
  "comb att" >:: (fun _ -> assert_equal "char1 attacked char2!" (State.action (Fight ("char1","char2")) st1 |> State.output));

  (*Shop*)
  "shop invQ" >:: (fun _ -> assert_equal "Invalid item quantity." (State.action (Buy("char1","item1","nope")) st2 |> State.output));
  "shop invC" >:: (fun _ -> assert_equal "Action Failed: Invalid character name." (State.action (Buy("c1","item1","4")) st2 |> State.output));
  "shop invI" >:: (fun _ -> assert_equal "Action Failed: That item is not available." (State.action (Buy("char1","it","4")) st2 |> State.output));
  "shop invN" >:: (fun _ -> assert_equal "Action Failed: There are only 3 available." (State.action (Buy("char1","item1","4")) st2 |> State.output));
  "shop invE" >:: (fun _ -> assert_equal "Action Failed: There is no shop here." (State.action (Buy("char1","item1","2")) st1 |> State.output));
  "shop buy" >:: (fun _ -> assert_equal "Items bought." (State.action (Buy("char1","item1","2")) st2 |> State.output));
  "shop buyI" >:: (fun _ -> assert_equal "Items bought." (State.action (Buy("char1","item2","200")) st2 |> State.output));
]

let suite =
  "Adventure test suite"
  >::: List.flatten [
    event_tests;
    state_tests;
    character_tests;
  ]

let _ = run_test_tt_main suite
