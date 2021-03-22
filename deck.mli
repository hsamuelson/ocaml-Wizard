type deck

val deal : deck -> int -> int -> Card.card_list list*Card.card

val shuffle : deck -> deck