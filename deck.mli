type deck

val deal : deck -> int -> int -> Card.card list list * Card.card

val shuffle : deck -> deck
