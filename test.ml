open OUnit2
open Event
open Character
open Global
open State

let evtC1 = Event.make_event "evtC1" Battle []

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
  equipped=[];
  inv=[item2];
}

let char2:Character.c = {
  name="char2";
  status="";
  race = Halfling;
  c_class = Sorcerer;
  wisdom=0;
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
  equipped=[];
  inv=[item1];
}

let evtC2 = Event.make_event "evtC1" Battle [(item1,Int 1)]

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
  "inv" >:: (fun _ -> assert_equal [item2] (Character.inv char1));
  "add item" >:: (fun _ -> assert_equal [item1;item2]
                     (Character.inv (Character.add_item char1 item1)));
  "equipped stuff" >:: (fun _ -> assert_equal [] (Character.equipped char1));
  "good equip" >:: (fun _ -> assert_equal [item2]
                       (Character.equipped (Character.equip char1 item2)));
  "bad equip" >:: (fun _ -> assert_equal []
                      (Character.equipped (Character.equip char1 item1)));
]

let event_tests = [
  "form" >:: (fun _ -> assert_equal (Battle:Event.form) (Event.get_form evtC1));
  "name" >:: (fun _ -> assert_equal "evtC1" (Event.get_name evtC1));
  "get_items" >:: (fun _ -> assert_equal [] (Event.get_items evtC1));
  "add_item" >:: (fun _ -> assert_equal [(item1,Int 1)] (Event.add_item item1 (Int 1) evtC1 |> Event.get_items));
  "changeF" >:: (fun _ -> assert_equal (Interaction:Event.form) (Event.change_form Interaction evtC1 |> Event.get_form));
]

let evtC3 = Event.make_event "evtC1" Battle [(item1, Int 1)]

let combat_tests = [
]

let suite =
  "Adventure test suite"
  >::: List.flatten [
    event_tests;
    combat_tests;
    character_tests;
  ]

let _ = run_test_tt_main suite
