type t = {
  total_deck : Card.card_list;
  unplayed_cards : Card.card_list;
}

let init main_deck =
  { total_deck = main_deck; unplayed_cards = main_deck }

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

let count_cards_better_than num suit cards trump_suit =
  if suit = trump_suit then
    List.fold_left
      (fun x y ->
        if Card.get_suit y = suit && Card.get_num y > num then x + 1
        else x)
      0 cards
  else
    List.fold_left
      (fun x y ->
        if
          (Card.get_suit y = suit && Card.get_num y > num)
          || Card.get_suit y = trump_suit
        then x + 1
        else x)
      0 cards

let odds_of_card_winning card trump_card unplayed_card_list player_cards
    =
  let trump_suit = Card.get_suit trump_card in
  let unplayed_cards = Card.get_cards unplayed_card_list in
  let cards_player = Card.get_cards player_cards in
  let card_suit = Card.get_suit card in
  if Card.get_num card = 14 then 0.
  else if Card.get_num card = 0 then 1.
  else if card_suit = trump_suit then
    let num_better_cards =
      count_cards_better_than (Card.get_num card) card_suit
        unplayed_cards trump_suit
    in
    let num_better_cards_in_hand =
      count_cards_better_than (Card.get_num card) card_suit cards_player
        trump_suit
    in
    float_of_int (num_better_cards + num_better_cards_in_hand)
    /. float_of_int
         (Card.get_cards_size unplayed_card_list
         + Card.get_cards_size player_cards
         - 1)
  else
    let num_better_cards =
      count_cards_better_than 0 trump_suit unplayed_cards trump_suit
      + count_cards_better_than (Card.get_num card) card_suit
          unplayed_cards trump_suit
    in
    let num_better_cards_in_hand =
      count_cards_better_than 0 trump_suit cards_player trump_suit
      + count_cards_better_than (Card.get_num card) card_suit
          cards_player trump_suit
    in
    float_of_int (num_better_cards + num_better_cards_in_hand)
    /. float_of_int
         (Card.get_cards_size unplayed_card_list
         + Card.get_cards_size player_cards
         - 1)
