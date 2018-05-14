open Global

module type Database = sig

  (* [data] is the type of the data *)
  type data

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> data

  (** [save_data f d] writes a data object to a file [f]  *)
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

module Database:Database
