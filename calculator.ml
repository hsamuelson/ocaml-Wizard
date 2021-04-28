type t = {
  total_deck : Card.card_list;
  unplayed_cards : Card.card_list;
}

let init main_deck =
  { total_deck = main_deck; unplayed_cards = main_deck }

let rec update_unplayed_helper calc card = failwith "unimplimented"

let update_unplayed calc (card : Card.card) =
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
