open Global

(** A [Command] is a player input that calls an event, retrieves information,
    manipulates the state or characer, or otherwise causes the gamespace to
    begin changing according to the client's actions.*)

  (* command is an imcomplete variant of command possibilities, which will be
     updated as more functionality is needed.  *)
     type command =
       |Nothing
       |Load of string
       |Save
       |Quit
       |Help of string
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
       |Sell of (string * string *string)
       |Fight of (string * string)
       |Cast of (string * string * string list)
       |Turn
       |QuickBuild of string list
       |CharacterAction
       |StateChange of (string * string)
       |ItemChange of (string * string)
       |Roll of (string)
       |UseItem of (string*string)
       |GetCharacterList of role
       |GetExits
       |Invalid


  (** [parse str] is the command parsed from the player input [str].
      [parse] requires any string, which will be parsed into one of the commands
      above, defaulting to Invalid for any non-specific string (this invalid is
      not the same as a command being invalid according to the game state. A
      valid string parsed into a command may not be accepted by state if there
      is no valid way to use that command in State).*)
  val parse : string -> command
