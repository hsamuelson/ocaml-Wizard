(* 0 is N, which always loses, and 14 is Wizard, which always wins*)
type number = int

type suit =
  | Red
  | Blue
  | Green
  | Yellow
  | No_Trump
  | No_Card

type card = {
  number : number;
  suit : suit;
}

type card_list = {
  cards : card list;
  size : int;
}

let get_cards card_list = card_list.cards

let get_cards_size card_list = card_list.size

let set_cards card_list cards = { card_list with cards }

let make_no_trump () = { number = 0; suit = No_Trump }
