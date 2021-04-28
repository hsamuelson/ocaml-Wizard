type t

val init : Card.card_list -> t

val update_unplayed : t -> Card.card -> t
