
(* TODO: you can change this if you want but please keep it named
   Parsable with the type t. The functions are just suggestions,
   you can change that. Delete this comment before we submit. -Kerri *)
(* [Parsable] is a datatype that can be parsed into values,
   such as JSON. *)
module type Parsable = sig
  (* [t] is the type of the parsable *)
  type t

  (* [member s j] gives the member of the datatype [j] with
      name [s]. *)
  val member : string -> t -> t

  (* [to_bool j] converts value [j] of type t to a bool *)
  val to_bool : t -> bool

  (* [to_int j] converts value [j] of type t to an int *)
  val to_int : t -> int

  (* [to_string j] converts value [j] of type t to a string *)
  val to_string : t -> string
end
