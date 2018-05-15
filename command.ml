open Global

  type command =
    |Nothing
    |Load of string
    |Save
    |Quit
    |Help of string
    |Event
    |LoadEvent of string
    |QuickEvent of (string*string)
    |Inquiry
    |Look
    |Move of string
    |Use of (string * string)
    |Inv of string
    |Give of string
    |Take of string
    |Drop of string
    |Shop of string
    |Buy of (string * string *string)
    |Sell of (string * string *string)
    |Fight of (string * string)
    |Cast of (string * string * string list)
    |Spell of string
    |Turn
    |QuickBuild of string list (* command entered: [name, class, race]*)
    |CharacterAction
    |StateChange of (string * string)
    |ItemChange of (string * string)
    |Roll of (string)
    |UseItem of (string*string)
    |Kill of string
    (* GETTERS  *)
    |GetCharacterList of role
    |GetExits
    |Whois of string
    |Equip of string*string
    |Invalid



  (* [starts_with start str] returns true if str begins with with start,
     false otherwise *)
  let starts_with start str  =
    if String.length start > String.length str then false else
    if String.sub str 0 (String.length start) = start then true else false


  (* [remove_start start str] returns str with start removed from the beginning,
    or str if start is longer than str*)
  let remove_start start str  =
    if String.length start > String.length str then str else
      let len = String.length start in
      String.trim (String.sub str len ((String.length str) - len))

  (*TODO what would you have to type to do item/state changes or character/entity
    creations??*)

let parse s =
  let str = (String.lowercase_ascii s |> String.trim) in
  if not (String.contains str ' ') then
    (* Single Word Commands *)
    begin match str with
    | "save" -> Save
    | "quit" -> Quit
    | "help" -> Help ("")
    | "inq" -> Inquiry
    | "look" -> Look
    | "inquiry" -> Inquiry
    | "turn" -> Turn
    |"event" -> Event
    | "characters" -> GetCharacterList (All)
    | _ -> Invalid end
  else let indx = (String.index str ' ') in
    let first = (String.sub str 0 indx) in
    let rest = (String.sub str (indx) ((String.length str)-indx))|>String.trim in
    (* Multiple Word Commands. "rest" is the remainder of the string.*)
    match first with
    |"load" -> Load rest
    |"loadevent" -> LoadEvent rest
    |"give" -> Give rest
    |"take" -> Take rest
    |"drop" -> Drop rest
    |"shop" -> Shop rest
    |"help" -> Help rest
    |"move" -> Move rest
    | "inv" -> Inv rest
    | "inventory" -> Inv rest
    |"get" ->
      let indxget = if (String.contains rest ' ')
              then (String.index rest ' ') else String.length rest in
      let firstget = String.sub rest 0 indxget in
      let restget = (String.sub rest (indxget) ((String.length rest) - indxget))
                    |>String.trim in
      (* Matching against "get ___ ". "restget" is the remainder.*)
      begin match firstget with
      |"characters" -> begin match restget with
        |"party" -> GetCharacterList (Party)
        |"hostile" -> GetCharacterList (Hostile)
        |"friendly" -> GetCharacterList (Friendly)
        |"neutral" -> GetCharacterList (Neutral)
        |_ -> GetCharacterList (All) end
      |"exits" -> GetExits
      |_ -> Invalid end
    |"use" -> let x = (remove_start "use" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      if List.length lst = 2 then
        match lst with
        | a::b::[] -> Use (a,b)
        | _ -> Invalid
      else Invalid
    |"roll" -> let x = (remove_start "roll" s) in Roll x
    |"fight" -> let x = (remove_start "fight" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
      | a::t::_ -> Fight (a,t)
      | _ -> Invalid end
    |"quickevent" -> let x = (remove_start "quickevent" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
        | a::b::_ -> QuickEvent (a,b)
        | _ -> Invalid end
    |"buy" -> let x = (remove_start "buy" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
      | c::i::q::[] -> Buy (c,i,q)
      | _ -> Invalid end
    |"sell" -> let x = (remove_start "sell" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
        | c::i::q::[] -> Sell (c,i,q)
        | _ -> Invalid end
    |"cast" -> let x = (remove_start "cast" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
        begin match lst with
        | c::s::t -> Cast (c,s,t)
        | _ -> Invalid end
    |"quickbuild" -> let x = (remove_start "quickbuild" s)  in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      QuickBuild lst
    | "who" -> let x = (remove_start "who is" s) in Whois x
    | "whomst" -> let x = (remove_start "whomst" s) in Whois x
    |"kill" -> let x = (remove_start "kill" s) in Kill x
    | "spell" -> let x = (remove_start "spell" s) in Spell x
    |"equip" -> let x = (remove_start "equip" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
        | a::b::_ -> Equip (a,b)
        | _ -> Invalid end
    |_ -> Invalid
