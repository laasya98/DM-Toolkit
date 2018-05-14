open Global
open Csv
module type Database = sig

  (* [data] is the format of the data *)
  type data

  (** [newdata] is the format of data with headers *)
  type newdata

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> newdata

  (** [save_data d f] writes a data object to a file [f]  *)
  val save_data : string -> data -> unit

  (** [hit_die class] is the hit die belonging to a given class
      represented by the string [class]
  *)
  val hit_die : string -> int

  (** [primary_stat class] is a tuple of the two most prominent
      (highest number) ability score of a given class
  *)
  val primary_stat : string -> (string * string)

  (** [speed_of race] is the speed stat of each race where [race]
      is a string
  *)
  val speed_of : string -> int

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

  type newdata = (string * string) list list
  (** [index] stores two association lists

  *)
  type index = {
        files : (string * string) list;
        other : (string * string) list;
  }

  let index = {
                files = [("init_state", "data/init_state.csv")];
                other = [("class_data", "data/classes.csv")];
              }

  let change_file field new_file =
    (field, new_file)::(List.remove_assoc field index.files)

  let load_data s = let tab = Csv.load s in
    Csv. associate (List.hd tab) (List.tl tab)

  let save_data f d = Csv.save f d

  let get typ index field file =
    let d = load_data file in
      List.find (fun l -> List.assoc typ l = index) d
        |> List.assoc field

  let hit_die c = get "class" c "hit_die" "class_data" |> int_of_string

  let tup_from_list l =
    ((try List.nth l 0 with _ -> ""), (try List.nth l 1 with _ -> ""))

  let primary_stat c = get "class" c "" "" |> String.split_on_char ' '
                       |> tup_from_list
  let speed_of r = get "race" r "speed" "race_data" |> int_of_string

  let get_item s = failwith "unimplemented"
  let get_npc s = failwith "unimplemented"
  let get_class s = failwith "unimplemented"
end
