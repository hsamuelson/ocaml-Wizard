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
