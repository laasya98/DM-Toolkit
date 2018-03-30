

module type Database = sig
  (* [data] is the type of the data *)
  type data

  (* [load_data f] is the data object retrieved from file [f] *)
  val load_data : string -> data

  (** [save_data d f] writes a data object to a file [f]  *)
  val save_data : data -> string 

(** [get_item id] is an item object corresponding to [id] in a
  * data object *)
  val get_item : string -> Item.i

  (** [get_npc id] is an npc object corresponding to [id] in a
    * data object *)
  val get_npc : string -> Event.npc

  (** [get_class id] is a D&D class corresponding to [id] in a
    * data object *)
    val get_class : string -> string

end
