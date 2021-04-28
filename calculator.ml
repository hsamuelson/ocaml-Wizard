type t = {
  total_deck : Card.card_list;
  unplayed_cards : Card.card_list;
  player_cards : Card.card_list;
}

let init main_deck =
  {
    total_deck = main_deck;
    unplayed_cards = main_deck;
    player_cards = Card.make_card_list [] 0;
  }

let get_unplayed calc = calc.unplayed_cards

let rec remove_card lst card acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if Card.equals h card then acc @ t
      else remove_card t card acc @ [ h ]

let rec update_unplayed_helper (calc : t) card : Card.card_list =
  let new_list =
    remove_card (Card.get_cards calc.unplayed_cards) card []
  in
  Card.make_card_list new_list
    (Card.get_cards_size calc.unplayed_cards - 1)

let update_unplayed (calc : t) (card : Card.card) =
  let new_unplayed = update_unplayed_helper calc card in
  { calc with unplayed_cards = new_unplayed }

let count_cards_better_than num suit cards =
  List.fold_left
    (fun x y ->
      if Card.get_suit y = suit && Card.get_num y > num then x + 1
      else x)
    0 cards

let odds_of_card_winning card trump_card unplayed_card_list =
  let unplayed_cards = Card.get_cards unplayed_card_list in
  if Card.get_suit card = Card.get_suit trump_card then
    let num_better_cards =
      count_cards_better_than (Card.get_num card) (Card.get_suit card)
        unplayed_cards
    in
    float_of_int num_better_cards
    /. float_of_int (Card.get_cards_size unplayed_card_list)
  else
    let num_better_cards =
      count_cards_better_than 0
        (Card.get_suit trump_card)
        unplayed_cards
      + count_cards_better_than (Card.get_num card) (Card.get_suit card)
          unplayed_cards
    in
    float_of_int num_better_cards
    /. float_of_int (Card.get_cards_size unplayed_card_list)
