open Database
open Event
open Character
open Command
open Global

module D = Database
module C = Character
module E = Event
module Com = Command

type character = Character.c
type event = Event.t
type command = Com.command
type data = D.data

type entity =
  |Item of item
  |Event of event

type location = {
  name : string;
  description : string;
  characters : (character * role) list;
  items : item list;
  event : event;
  exits : ( string * string ) list (*(direction, location)*)
}

type state = {
  locations : location list;
  characters : (character * role) list;
  event : event;
  output : string;
  current_location : location;
}

let add_filename n data :unit =
  try
    let f = find_assoc n data in
    D.change_file n f
  with _ -> ()

let list_of_string s =
  String.sub s 1 (String.length s - 2) |> String.split_on_char ';'

let exits_of_string s = failwith ""

let parse_characters data =
    let role r = match r with
    |"Party" -> Party
    |"Friendly" -> Friendly
    |"Hostile" -> Hostile
    |_ -> Neutral
  in
  let cs = String.split_on_char ' ' data in
  let split_char s =
    let indxget = String.index s ':' in
    let c = String.sub s 0 indxget in
    let r = (String.sub s (indxget) ((String.length s) - indxget))
            |>String.trim in
    (c,r)
  in
  let a = List.map (fun s -> split_char s) cs in
    let b =  List.map (fun (x,r) -> (D.get_char x,r)) a in
    List.map (fun (c,r) -> (C.parse_char c,role r)) b

  let parse_itemlst ilst =
    let ilst' = String.split_on_char '+' ilst in
    let get_item str = Database.get_item str |> parse_item
    in
    List.map get_item ilst'

  let parse_loc dlist =
    try
      let n = find_assoc "Name" dlist in
      let d = find_assoc "Description" dlist in
      let cs = (find_assoc "Characters" dlist) |> check_none parse_characters [] in
      let is = find_assoc "Items" dlist |> check_none parse_itemlst [] in
      let e = find_assoc "Event" dlist |> D.get_event |> E.parse_event in
      let exits = list_of_string (find_assoc "Exits" dlist) |> exits_of_string in
      {name=n; description=d; characters=cs; items=is; event=e; exits=exits}
    with x -> raise (Failure "Invalid Location File")

  let parse_locations data =
    let cs = String.split_on_char ' ' data in
    let csd = List.map (fun x -> D.get_location x) cs in
    List.map (fun c -> parse_loc c) csd

let add_data data =
  add_filename "class_data" data;
  add_filename "race_data" data;
  add_filename "item_data" data;
  add_filename "location_data" data;
  add_filename "spell_data" data;
  add_filename "char_data" data;
  add_filename "event_data" data

let empty_location = {
  name = "empty";
  description = "no description";
  characters = [];
  items = [];
  event = E.init_event "event";
  exits = [];
}

let empty_state = {
  locations = [empty_location];
  characters = [];
  event = init_event "";
  output = "Empty State Loaded.";
  current_location = empty_location;
}

let parse_event s =
    E.parse_event (D.get_event s)

let parse_curr_l l s =
  List.find (fun l -> l.name=s) l

let parse_state data =
  add_data data;
  try
    let l = find_assoc "Locations" data |> check_none parse_locations [empty_location] in
    let c = find_assoc "Characters" data |> check_none parse_characters [] in
    let e' = find_assoc "Event" data |> check_none parse_event (E.init_event "") in
    let curr_l' = find_assoc "Curr_Loc" data |> check_none (parse_curr_l l) empty_location  in
    {locations=l;characters=c; event=e';output="State Loaded.";current_location=curr_l'}
  with d -> raise d

(** [alter_state] st currLoc evt chars output is a function for conveniently
  changing the fields in state and providing an output for main to display.*)
let alter_state st ?(currLoc=st.current_location)
    ?(evt=st.event) ?(chars=st.characters) output =
  {
    current_location = currLoc;
    locations = st.locations;
    event=evt;
    characters=chars;
    output=output;
  }

let init_state n = D.load_data n |> D.flatten |> parse_state

let current_location st = alter_state st st.current_location.name
let current_room_characters st = failwith "unimplemented"
let rooms st = failwith "unimplemented"
let event st = st.event

(** [get_exits st] returns a list of strings that represents the exits for the
    current room.*)
let get_exits st =
  List.map (fun x -> fst x) (st.current_location.exits)


(* [move st dir] is the state with current_room now the room reflected by dir
   from the original current_room. It leaves all non-party characters in the
   previous room. It's output is "Party moves *dir*" with a description*)
let move_dir st dir =
  if not (List.mem dir (List.map (fun x -> fst x) st.current_location.exits))
  then alter_state st "Not a direction"
  else
    let newlocation =
      snd (List.find (fun x -> fst x = dir) st.current_location.exits) in
    let nl = List.find_opt (fun l -> l.name = newlocation) st.locations in
    match nl with
    | None -> alter_state st "Not a valid direction."
    |Some loc ->
    let newcharacters =
      (List.filter (fun x -> snd x = Party) st.characters)
      @ loc.characters in
    alter_state st ~currLoc:loc ~chars:newcharacters
      ("Party moves" ^dir ^"\n\n\n" ^ loc.description)

(** [character_list_filter ls role] returns a string delimited by commas
  representing the characters in a list that match the given role *)
let character_list_filter ls role =
  ((List.filter (fun x -> snd x = role) ls)
   |> List.map (fun x -> C.name (fst x)))

let character_list_string ?role:(r = All) st =
  match r with
  |All -> (List.map (fun (x,y) -> C.name x) st.characters) |> String.concat ", "
  |role -> character_list_filter st.characters role |> String.concat ", "


(*still needs to add remove_item and support giving more than one item*)
let give st ?(q=1) item p1 p2 =
  try
    let c1 = (List.find (fun x -> fst x = p1) st.characters) in
    let c1' = (C.remove_item (fst c1) item q), snd c1 in
    let c2 = (List.find (fun x -> fst x = p2) st.characters) in
    let c2' = (C.add_item (fst c2) item q), snd c2 in
    let clist = (List.filter (fun x -> (fst x <> p1)) st.characters) in
    let clist' = (List.filter (fun x -> (fst x != p2)) clist) in
    alter_state {st with characters = c1'::c2'::clist'} (item.name ^ "given")
  with _ -> alter_state st ("One or more items or characters missing")

let update_char c c' ?(r'=None) st =
match r' with
| None -> List.map (fun (x,r) -> if x=c then (c',r) else (x,r)) st.characters
| Some r' ->
  List.map (fun (x,r) -> if x=c then (c',r') else (x,r)) st.characters

let update_chars cs st =
  let rec r cs lst =
  match cs with
  | [] -> lst
  | c::t -> r t (List.map
     (fun (x,r) -> if C.name x = C.name c then (c,r)
      else (x,r)) lst)
  in
  r cs st.characters

(** SHOP **)

let buy c i q evt st =
  let m = i.value * q in
  let cm = C.money c in
  if cm < m then alter_state st "Character doesn't have enough money."
  else
    let c' = C.update_money (C.add_item c i q) (cm-m) in
    let evt' = E.remove_item i.name (Int q) evt in
    alter_state st ~evt:evt' ~chars:(update_char c c' st) "Items bought."

let sell c i q evt st =
  let m = i.value * q / 10 in
  let cm = C.money c in
  let c' = C.update_money (C.remove_item c i q) (cm+m) in
  let evt' = E.add_item i (Int q) evt in
  alter_state st ~evt:evt' ~chars:(update_char c c' st) "Items sold."

let use c i q evt st =
  let (evt', t') = E.use_item i c evt in
  let chars = update_chars t' st in
  alter_state st ~evt:evt' ~chars:chars (E.verbose evt')

let item_helper c i q inv f evt st =
  match List.find_opt (fun ((x:character),_) -> x.name = c) st.characters with
  | None -> alter_state st "Action Failed: Invalid character name."
  | Some (c,_) ->
    let stock = if inv then C.inv c else E.get_items evt in
    match List.find_opt (fun ((x:item),_) -> x.name =i) stock with
    | None ->  alter_state st "Action Failed: That item is not available."
    | Some (i, Infinity) -> f c i q evt st
    | Some (i, Int n) ->
      if n<q then
        alter_state st ("Action Failed: There are only "^(string_of_int n)^" available.")
      else
        f c i q evt st

let b_s_item c i q evt st buying =
  match E.get_form st.event with
  | Shop -> begin
      try
        let q' = int_of_string q in
        if buying then
          item_helper c i q' false buy evt st
        else
          item_helper c i q' true sell evt st
    with _ -> alter_state st "Invalid item quantity."
  end
  |_ -> alter_state st "Action Failed: There is no shop here."

let use_item i c evt st= item_helper c i 1 true use evt st

(** COMBAT **)

let attack a t evt st:state =
  let ac = List.find_opt (fun (x,_) -> C.name x = a) st.characters in
  let tc = List.find_opt (fun (x,_) -> C.name x = t) st.characters in
  match ac with
  | None -> alter_state st "Action Failed: Invalid Attacker Name"
  | Some (a,_) ->
    match tc with
    | None -> alter_state st "Action Failed: Invalid Target Name"
    | Some (t,_) -> let (evt', t') = E.attack a t evt in
      let chars = update_char t t' st in
      alter_state st ~evt:evt' ~chars:chars (E.verbose evt')

let char_by_name s st =
  let c = List.find_opt (fun (x,_) -> C.name x = s) st.characters in
  match c with
  | None -> None
  | Some (x,_) -> Some x

let cast c s t evt st =
  let c' = char_by_name c st in
  let s' = failwith "match s with a spell in char spells" in
  try
    let find s = List.find (fun (x,_) -> C.name x = s) st.characters in
    let t' = List.map (fun s -> fst (find s)) t in
    match c' with
    | None -> alter_state st "The castor name is invalid."
    | Some c ->
      let (evt', t') = E.cast c s' t' evt in
      let chars = update_chars t' st in
      alter_state st ~evt:evt' ~chars:chars (E.verbose evt')
  with _ -> alter_state st "One or more target names invalid."

let use_item i c evt st =
  let c' = char_by_name c st in
  match c' with
  | None -> alter_state st "Character name invalid."
  | Some c ->
    try
      let (i',_) = List.find (fun ((x:item ),n) -> x.name=i) (C.inv c) in
      let (evt', t') = E.use_item i' c evt in
      let chars = update_chars t' st in
      alter_state st ~evt:evt' ~chars:chars (E.verbose evt')
    with _ -> alter_state st "Item not found in character inventory."

let distribute_xp c st =
  match List.find_opt (fun (x,r) -> C.name x = c) st.characters with
  |None -> st
  |Some (_,Party) -> st
  |Some (m,_) ->
    let party = List.filter (fun (_,r) -> r=Party) st.characters in
    let xp = C.xp m / (List.length party) in
    let p' = party |> List.map (fun (x,_) -> C.update_xp x (C.xp x + xp)) in
    alter_state st ~chars:(update_chars p' st) "XP distributed."

let remove_char c st =
  let cs = List.filter (fun (x,_) -> C.name x <> c) st.characters in
  let evt' = remove_char c st.event in
  alter_state st ~chars:cs ~evt:evt' "Character removed."

let kill_char c st =
  let c' = char_by_name c st in
  match c' with
  | None -> alter_state st "Character name invalid."
  | Some c ->
    let st' = remove_char c.name (distribute_xp c.name st) in
    alter_state st' "Character killed."

let string_of_role r =
  match r with
  | All -> "All"
  | Party -> "Party"
  | Hostile -> "Hostile"
  | Friendly -> "Friendly"
  | Neutral -> "Neutral"

let print_char_short (c,r) =
  (C.name c)^"\t"^(string_of_role r)^"\tHP: "^(string_of_int (C.curr_hp c))

let print_chars_short st =
  List.map print_char_short st.characters
  |> List.fold_left (fun x a-> x^"\n"^a) ""

let gen_printout st =
  let evt = st.event in
  match E.get_form evt with
  |Battle ->
    (st.output)^
    "\n\nTurn number "^(string_of_int (E.get_turn evt))^".\n"
    ^(List.hd (E.get_turnlst evt))^"'s turn.\n\n"^(print_chars_short st)
  |Shop ->
    (st.output)^
    "\n\nAt "^(E.get_name evt)^"\nItems available: "^(String.concat ", " (E.get_item_names evt))
  |_ -> st.output

let action (c:command) (st:state) =
  E.clear_vout st.event;
  match c with
  | Fight (a,b) -> begin
    match E.get_form st.event with
    | Battle -> attack a b st.event st
    | _ -> alter_state st "No battle event occurring."
  end
  | Cast (c,s,t) -> cast c s t st.event st
  | UseItem (c,i) -> item_helper c i 1 true use st.event st
  | Buy (ch,i,q) -> b_s_item ch i q st.event st true
  | Sell (ch,i,q) -> b_s_item ch i q st.event st false
  | Turn ->begin
      match E.get_form st.event with
      |Battle ->
        let (evt', t') = E.turn st.event in
        let chars = update_chars t' st in
        alter_state st ~evt:evt' ~chars:chars ("Turn incremented.\n"^(E.verbose evt'))
      |_ -> alter_state st "No battle occuring."
  end
  | Move r -> (try move_dir st r with _-> alter_state st "No such direction.")
  | GetCharacterList r -> begin
      let lst = begin match r with
      |All -> character_list_string  st
      |role -> character_list_string ~role:role st
      end
    in alter_state st lst
  end
  |GetExits -> alter_state st (String.concat ", " (get_exits st))
  |Roll d -> alter_state st (string_of_int (Global.roll_dice_string d))
  |QuickBuild lst -> begin try
    let n = List.hd lst in
    let c = List.hd (List.tl lst) in
    let r = List.nth lst 2 in
    let newchar = C.quickbuild n c r in
    let x c = () in (x newchar);
    let newcharls = ((newchar,Party) :: st.characters) in
    alter_state st ~chars:newcharls ("New Character, " ^ n ^ ", added to party!")
      with _-> alter_state st "Failed. Your arguments might be off." end
  |QuickEvent (n,f) -> begin
      let f' = match f with
      |"battle" -> Battle
      |"shop" -> Shop
      | _ -> Interaction
      in
      let c = List.map fst st.characters in
      let evt = E.make_event n f' [] c in
      alter_state st ~evt:evt "Event started."
    end
  | Whois x -> begin
      let s =
      match char_by_name x st with
        | Some c -> C.details c
        | None -> "Invalid move. That's not a character"
      in alter_state st s
    end
  | Event -> alter_state st ("Current Event: "^(E.get_name st.event))
  | _ -> alter_state st "Invalid move. Try again. Type \"help commands\" for a list of commands"

let output st = st.output
