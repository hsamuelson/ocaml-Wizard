type card

type card_list

val get_cards : card_list -> card list

val get_cards_size : card_list -> int

val set_cards : card_list -> card list -> card_list

val make_no_trump : unit -> card

val make_no_card : unit -> card

val make_card : int -> string -> card

val make_card_list : card list -> int -> card_list
