(** Represents a calculator object that keeps track of all the played
    cards on the table and calculates the probability of your win.*)

(** Abstract representation of the calculator object*)
type t

(**[init a] returns an empty calculator to use in the rest of the round.
   Pass in [a] as a list of cards that haven't been played yet.*)
val init : Card.card_list -> t

(** [update_unplayed a b] returns a calculator object with the unplayed
    cards updated to represent the board. *)
val update_unplayed : t -> Card.card -> t

(** [odds_of_card_winning a b c d] returns the percentage of the deck
    that your card is better than. *)
val odds_of_card_winning :
  Card.card -> Card.card -> Card.card_list -> Card.card_list -> float

(** [get_unplayed t] returns all the unplayed cards in a deck from the
    calculator [t]*)
val get_unplayed : t -> Card.card_list
