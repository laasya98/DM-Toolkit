module type Database = sig

  (* [data] is the format of the data *)
  type data

  (** [index] stores a mapping from keywords to filenames *)
  type index

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> data

  (** [save_data f d] writes a data object to a file [f]  *)
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
  val change_file : string -> string -> unit

  (** TODO specify*)
  val get_item : string -> (string * string) list
  val get_location : string -> (string * string) list
  val get_event : string -> (string * string) list
  val get_char : string -> (string * string) list
end

module Database:Database
