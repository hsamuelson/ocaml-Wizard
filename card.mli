(**the abstract type value representing a card*)
type card

(**the abstract type value representing a card_list*)
type card_list

(**[get_cards] gives a list of cards in [card_list]*)
val get_cards : card_list -> card list

(**[get_cards_size] gives the size of [card_list]*)
val get_cards_size : card_list -> int

(**[set_cards] takes the inputted [card_list], replaces the current
   cards with [card list], and outputs a resultant [card_list]*)
val set_cards : card_list -> card list -> card_list

(**[make_no_trump] outputs a [card] with suit = "No_Trump" and number =
   0 that represents the trump card when there is to be no trump suit*)
val make_no_trump : unit -> card

(**[make_no_card] outputs a [card] with suit = "None" and number = 0
   that represents an undefined card*)
val make_no_card : unit -> card

(**[make_card] makes a [card] with the number = [int] and suit =
   [string]*)
val make_card : int -> string -> card

(**[make_card_list] makes a [card_list] with cards = [card list] and
   number =[int]*)
val make_card_list : card list -> int -> card_list

(**[string_of_card] card outputs a string representation of a card, e.g.
   a card with number = 0 and suit = "Red" would output "[ 0 , Red ]"*)
val string_of_card : card -> string
