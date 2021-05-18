(**Represents a printer object to print cards graphically*)

(**[intro_screen] prints the intro screen graphic for the wizard game*)
val intro_screen : unit -> unit

(**[print_hand] prints the graphic representing card list [c_list]*)
val print_hand : Card.card list -> int -> unit
