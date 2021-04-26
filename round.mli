(** The abstract type of values representing a round *)
type t

val init_first_round : int -> Card.card_list -> Player.t list -> t

val play_round : t -> t

val scoreboard : Player.t list -> string * string

val players : t -> Player.t list
val round_num : t ->  int 
val deck_size : t -> int

val print_scoreboard : t -> unit