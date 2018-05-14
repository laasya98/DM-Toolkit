open Global
open Csv
module type Database = sig

  (* [data] is the type of the data *)
  type data

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> data

  (** [save_data d f] writes a data object to a file [f]  *)
  val save_data : string -> data -> unit

  (** [get_item id] is an item object corresponding to [id] in a
    * data object *)
  val get_item : string -> item

  (** [get_char id] is an character object corresponding to [id] in a
    * data object *)
  val get_npc : string -> data

  (** [get_class id] is a D&D class corresponding to [id] in a
    * data object *)
  val get_class : string -> string
end

module Database = struct
  type data = string list list

  let load_data s = Csv.load s


  let save_data f d = Csv.save f d


  let get_item s = failwith "unimplemented"
  let get_npc s = failwith "unimplemented"
  let get_class s = failwith "unimplemented"
end
