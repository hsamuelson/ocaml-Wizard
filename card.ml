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
  "[ " ^ string_of_int card.number ^ " , " ^ card.suit ^ " ]"

let get_num card = card.number

let get_suit card = card.suit

let add_card_to_list lst card =
  let old_cards = lst.cards in
  let old_size = lst.size in
  { cards = card :: old_cards; size = old_size + 1 }

let equals (card1 : card) (card2 : card) =
  card1.number = card2.number && card1.suit = card2.suit
