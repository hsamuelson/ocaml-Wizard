(**[card] is the type value representing a card*)
type card = {
  number : int;
  suit : string;
}

(**[card_list] is the type value representing a card_list*)
type card_list = {
  cards : card list;
  size : int;
}

(**[get_cards] gives a list of cards in [card_list]*)
let get_cards card_list = card_list.cards

(**[get_cards_size] gives the size of [card_list]*)
let get_cards_size card_list = card_list.size

(**[set_cards] takes the inputted [card_list], replaces the current
   cards with [card list], and outputs a resultant [card_list]*)
let set_cards card_list cards = { card_list with cards }

(**[make_no_trump] outputs a [card] with suit = "No_Trump" and number =
   0 that represents the trump card when there is to be no trump suit*)
let make_no_trump () = { number = 0; suit = "No_Trump" }

(**[make_no_card] outputs a [card] with suit = "None" and number = 0
   that represents an undefined card*)
let make_no_card () = { number = 0; suit = "No_Card" }

(**[make_card] makes a [card] with the number = [int] and suit =
   [string]*)
let make_card number suit = { number; suit }

(**[make_card_list] makes a [card_list] with cards = [card list] and
   number =[int]*)
let make_card_list cards size = { cards; size }

(**[string_of_card] card outputs a string representation of a card, e.g.
   a card with number = 0 and suit = "Red" would output "[ 0 , Red ]"*)
let string_of_card card =
  "[ " ^ string_of_int card.number ^ " , " ^ card.suit ^ " ]"

(**[get_num] returns the number of the card *)
let get_num card = card.number

(**[get_suit] returns the suit of the card*)
let get_suit card = card.suit

(**[add_card_to_list] returns a [card_list] with [card] added to the
   front of the list, and size incremented by 1*)
let add_card_to_list lst card =
  let old_cards = lst.cards in
  let old_size = lst.size in
  { cards = card :: old_cards; size = old_size + 1 }

(**[equals] returns true if two cards have the same suit and number*)
let equals (card1 : card) (card2 : card) =
  card1.number = card2.number && card1.suit = card2.suit
