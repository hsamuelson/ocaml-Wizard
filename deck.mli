(** Represents a deck of card objects.*)

(*[deal] deals a deck of cards into [num_players] different card lists
  where each card list has [round_number] of cards, and outputs a trump
  card *)
val deal :
  Card.card_list -> int -> int -> Card.card list list * Card.card

(*[shuffle] sorts [deck]'s cards randomly and returns a shuffled deck*)
val shuffle : Card.card_list -> Card.card_list

(**[make_deck] returns a deck from the given [json]*)
val make_deck : Yojson.Basic.t -> Card.card_list
