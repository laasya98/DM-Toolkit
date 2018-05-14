open Global
open Csv
module type Database = sig

  (* [data] is the format of the data *)
  type data

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> data

  (** [save_data d f] writes a data object to a file [f]  *)
  val save_data : string -> data -> unit

  (** [change_file field new_file] changes the default file
      for the type of query [field] to the filename [new_file]
  *)
  val change_file : string -> string -> (string * string) list

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

  (** [data] mirrors the structure of the Csv library's
      abstract type
      elements of inner list will be delimited by commas
      inner lists themselves will be delimited by newlines
  *)
  type data = string list list

  (** [index] stores two association lists

  *)
  type index = {
        files : (string * string) list;
        other : (string * string) list;
  }

  let index = {
                files = [("init_state", "data/init_state.csv")];
                other = [];
              }

  let change_file field new_file =
    (field, new_file)::(List.remove_assoc field index.files)

  let load_data s = Csv.load s

  let save_data f d = Csv.save f d



  let get_item s = failwith "unimplemented"
  let get_npc s = failwith "unimplemented"
  let get_class s = failwith "unimplemented"
end
