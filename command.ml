open Global

  type command =
    |Nothing
    |Load of string
    |Save
    |Quit
    |Help
    |Event of string
    |Inquiry
    |Move of (string *string)
    |Use of (string * string)
    |Inv
    |Give of string
    |Take of string
    |Drop of string
    |Shop of string
    |Buy of (string * string *string)
    |Fight of (string * string)
    |Cast of (string * string * string list)
    |Turn
    |QuickBuild of string list (* command entered: [name, class, race, (sub_race)]*)
    |CharacterAction
    |StateChange of (string * string)
    |ItemChange of (string * string)
    |Roll of (string * string)
    |UseItem of (string*string)
    |GetCharacterList of role
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
    String.trim (String.sub str (String.length start) (String.length str))

  (*TODO what would you have to type to do item/state changes or character/entity
    creations??*)

let parse s =
  let str = (String.lowercase_ascii s |> String.trim) in
  if not (String.contains str ' ') then
    (* Single Word Commands *)
    begin match str with
    | "save" -> Save
    | "quit" -> Quit
    | "help" -> Help
    | "inv" -> Inv
    | "inventory" -> Inv
    | "inq" -> Inquiry
    | "inquiry" -> Inquiry
    | "turn" -> Turn
    | "characters" -> GetCharacterList (All)
    | _ -> Invalid end
  else let indx = (String.index str ' ') in
    let first = (String.sub str 0 indx) in
    let rest = (String.sub str (indx) ((String.length str)-indx))|>String.trim in
    (* Multiple Word Commands. "rest" is the remainder of the string.*)
    match first with
    |"load" -> Load rest
    |"event" -> Event rest
    |"give" -> Give rest
    |"take" -> Take rest
    |"drop" -> Drop rest
    |"shop" -> Shop rest
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
      |_ -> Invalid end
    |"move" -> let x = (remove_start "move" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      if List.length lst = 2 then
        match lst with
        | a::b::[] -> Move (a,b)
        | _ -> Invalid
      else Invalid
    |"use" -> let x = (remove_start "use" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      if List.length lst = 2 then
        match lst with
        | a::b::[] -> Use (a,b)
        | _ -> Invalid
      else Invalid
    |"roll" -> let x = (remove_start "roll" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      if List.length lst = 2 then
        match lst with
        | a::b::[] -> Roll (a,b)
        | _ -> Invalid
      else Invalid
    |"fight" -> let x = (remove_start "fight" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
      | a::t::_ -> Fight (a,t)
      | _ -> Invalid end
    |"buy" -> let x = (remove_start "buy" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      begin match lst with
      | c::i::q::[] -> Buy (c,i,q)
      | _ -> Invalid end
    |"cast" -> let x = (remove_start "cast" s) in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
        begin match lst with
        | c::s::t -> Cast (c,s,t)
        | _ -> Invalid end
    |"quickbuild" -> let x = String.sub s 11 (String.length s)  in
      let lst = List.filter (fun x -> x <> "") (String.split_on_char ' ' x) in
      if List.length lst = 3 || List.length lst = 4 then
      QuickBuild lst else Invalid
    |_ -> Invalid
