(** The abstract type of values representing a round *)
type t

val init_first_round : int -> Card.card_list -> t
