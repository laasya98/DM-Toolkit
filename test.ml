open OUnit2
open Event
open Character
open Global

let evtC1 = Event.make_event "evtC1" Battle [] []

let char1:Character.c = {
  name="char1";
  status="";
  defense=0;
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
  inv=[];
}

let char2:Character.c = {
  name="char2";
  status="";
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
  inv=[];
}
let npc1:Event.npc = {details=char1; role=Neutral; tag=0}
let npc2:Event.npc = {details=char2; role=Hostile; tag=0}

let item1 = {
  name="item1";
  i_type=Other;
  description = "item1 desc";
  weight = 1;
  effect = {stat=""; amount=0};
  uses = 0;
  value=10;
}

let evtC2 = Event.make_event "evtC1" Battle [npc1] [item1]

let event_tests = [
  "form" >:: (fun _ -> assert_equal (Battle:Event.form) (Event.get_form evtC1));
  "name" >:: (fun _ -> assert_equal "evtC1" (Event.get_name evtC1));
  "npcs" >:: (fun _ -> assert_equal [] (Event.get_npcs evtC1));
  "get_items" >:: (fun _ -> assert_equal [] (Event.get_items evtC1));
  "add_item" >:: (fun _ -> assert_equal [item1] (Event.add_item item1 evtC1 |> Event.get_items));
  "changeF" >:: (fun _ -> assert_equal (Interaction:Event.form) (Event.change_form Interaction evtC1 |> Event.get_form));
  "add_npc" >:: (fun _ -> assert_equal [npc1] (Event.add_npc char1 Neutral evtC1 |> Event.get_npcs));
  "remove_npc" >:: (fun _ -> assert_equal [] (Event.remove_npc 0 evtC1 |> Event.get_npcs));
  "update_npc" >:: (fun _ -> assert_equal [npc2] (Event.update_npc npc1 ~c:char2 ~r:Hostile evtC2 |> Event.get_npcs));
]

let evtC3 = Event.make_event "evtC1" Battle [npc1; npc2] [item1]
let at1 = Event.attack_opt None "char1" None "char2" evtC3

let combat_tests = [
  "a1" >:: (fun _ -> assert_equal None (snd at1));
  "a2" >:: (fun _ -> assert_equal "char2 was attacked. NPC status updated." (fst at1 |> Event.get_output));
]

let suite =
  "Adventure test suite"
  >::: List.flatten [
    event_tests;
    combat_tests;
  ]

let _ = run_test_tt_main suite
