open Csv
open Hashtbl

module type Database = sig

  (* [data] is the format of the data *)
  type data

  (** [index] stores a mapping from keywords to filenames *)
  type index

  val flatten : data -> (string * string) list

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> data

  (** [save_data d f] writes a data object to a file [f]  *)
  val save_data : string -> string list list -> unit

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
  val change_file : string -> string -> unit

  (** TODO specify*)
  val get_item : string -> (string * string) list
  val get_location : string -> (string * string) list
  val get_event : string -> (string * string) list
  val get_char : string -> (string * string) list
  val prof_of_level : int -> int
  val get_spell_data : string -> (string * string) list
  val xp_from_level : int -> int
end

module Database = struct

  (** [data] mirrors the structure of the Csv library's
      abstract type with headers
      elements of inner list will be delimited by commas
      inner lists themselves will be delimited by newlines
  *)
  type data = (string * string) list list

  (** [index] is a hash table of association  *)
  type index = (string, string) Hashtbl.t

  let files = create 5

  let () =
    Hashtbl.add files "class_data" "./data/classes.csv";
    Hashtbl.add files "race_data" "./data/races.csv";
    Hashtbl.add files "level_data" "./data/level_up.csv";
    Hashtbl.add files "spell_data" "./data/spells.csv";
    Hashtbl.add files "loc_data" "./data/locations.csv";
    Hashtbl.add files "char_data" "./data/char_data.csv";
    Hashtbl.add files "event_data" "./data/events.csv"


  let change_file field new_file =
    Hashtbl.add files field new_file

  let flatten data = List.flatten data

  let load_data s =
    let d =
        Hashtbl.find files s in
    let tab = Csv.load d in
    Csv.associate (List.hd tab) (List.tl tab)


  let save_data f d =
    Unix.mkdir f 0o640;
    Csv.save ("newstate.csv") d


  (** []  *)
  let get_lst typ ind file =

      let d = load_data file in

    let idk = fun s -> s |> String.trim |> String.lowercase_ascii in
      List.find (fun l -> idk (List.assoc typ l) = idk ind) d

  let get typ ind field file =
    get_lst typ ind file |> List.assoc field

  let hit_die c =
    get "class" c "hit_die" "class_data"
                  |> int_of_string

  let tup_from_list l =
    ((try List.nth l 0 with _ -> ""), (try List.nth l 1 with _ -> ""))

  let primary_stat c =
    get "class" c "primary" "class_data"
                       |> String.split_on_char ' '
                       |> tup_from_list

  let speed_of r =
    get "Race" r "Speed" "race_data"
                       |> int_of_string

  let get_item s = get_lst "Name" s "item_data"
  let get_location s = get_lst "Name" s "loc_data"
  let get_event s = get_lst "Name" s "event_data"
  let get_char s = get_lst "Name" s "char_data"
  let prof_of_level i =
    get "level" (string_of_int i) "prof" "level_data"
                         |> int_of_string
  let get_spell_data s = get_lst "Name" s "spell_data"

  let xp_from_level i = get "level" (string_of_int i) "exp" "level_data"
                      |> int_of_string

end
