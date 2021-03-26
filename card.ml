(* 0 is N, which always loses, and 14 is Wizard, which always wins*)
(* | Red | Blue | Green | Yellow | No_Trump | No_Card *)

type card = {
  number : int;
  suit : string;
}

type card_list = {
  cards : card list;
  size : int;
}

let get_cards card_list = card_list.cards

let get_cards_size card_list = card_list.size

let set_cards card_list cards = { card_list with cards }

let make_no_trump () = { number = 0; suit = "No_Trump" }

let make_no_card () = { number = 0; suit = "No_Card" }

let make_card number suit = { number; suit }

let make_card_list cards size = { cards; size }

let string_of_card card =
  let num = string_of_int card.number in
  let suit = card.suit in
  "[ " ^ num ^ " , " ^ suit ^ " ]"

let get_num card = card.number
