type t

val init : Card.card_list -> t

val update_unplayed : t -> Card.card -> t

val odds_of_card_winning :
  Card.card -> Card.card -> Card.card_list -> int -> float
