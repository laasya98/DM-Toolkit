module type Command = sig

  (* command is an imcomplete variant of command possibilities, which will be
     updated as more functionality is needed.  *)
  type command =
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
    |Fight of (string * string)
    |CreateEntity
    |CharacterAction
    |StateChange of (string * string)
    |ItemChange of (string * string)
    |Roll of (string * string)
    |Invalid

  val parse : string -> command
end


module Command = struct
  type command =
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
    |Fight of (string * string)
    |CreateEntity
    |CharacterAction
    |StateChange of (string * string)
    |ItemChange of (string * string)
    |Roll of (string * string)
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
    match (String.lowercase_ascii s) with
    | "save" -> Save
    | "quit" -> Quit
    | "help" -> Help
    | "inv" -> Inv
    | "inventory" -> Inv
    | "inq" -> Inquiry
    | "inqury" -> Inquiry
    | _ ->  if (starts_with "load" s) then Load (remove_start "load" s)
      else if (starts_with "event" s) then Event (remove_start "event" s)
      else if (starts_with "give" s) then Give (remove_start "give" s)
      else if (starts_with "take" s) then Take (remove_start "take" s)
      else if (starts_with "drop" s) then Drop (remove_start "drop" s)
      else if (starts_with "shop" s) then Shop (remove_start "shop" s)
      else if (starts_with "move" s) then
        let x = (remove_start "move" s) in
        let lst = String.split_on_char ' ' x in
        if List.length lst = 2 then
          match lst with
          | a::b::[] -> Move (a,b)
          | _ -> Invalid
        else Invalid
      else if (starts_with "use" s) then
        let x = (remove_start "use" s) in
        let lst = String.split_on_char ' ' x in
        if List.length lst = 2 then
          match lst with
          | a::b::[] -> Use (a,b)
          | _ -> Invalid
        else Invalid
      else if (starts_with "roll" s) then
        let x = (remove_start "roll" s) in
        let lst = String.split_on_char ' ' x in
        if List.length lst = 2 then
          match lst with
          | a::b::[] -> Roll (a,b)
          | _ -> Invalid
        else Invalid
      else if (starts_with "fight" s) then
        let x = (remove_start "fight" s) in
        let lst = String.split_on_char ' ' x in
        match lst with
        | a::t::_ -> Fight (a,t)
        | _ -> Invalid
      else Invalid
end
