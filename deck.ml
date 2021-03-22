type deck = Card.card_list

type s = Card.suit

(*[random_compare] randomly returns 1 or -1 as a comparator value*)
let random_compare (a : Card.card) (b : Card.card) =
  let is_positive = Random.int 2 in
  if is_positive = 1 then 1 else -1

(*[shuffle] sorts [deck]'s cards randomly and returns a shuffled deck*)
let shuffle (deck : deck) =
  let cards = Card.get_cards deck in
  let shuffled_cards = List.sort random_compare cards in
  Card.set_cards deck shuffled_cards

(*[sublist] returns the sublist of [list] from [index1] inclusive to
  [index2] non-inclusive*)
let rec sublist list index1 index2 acc counter =
  if counter < index1 then sublist list index1 index2 acc index1
  else if counter >= index2 then acc
  else
    let acc = List.nth list counter :: acc in
    sublist list index1 index2 acc (counter + 1)

(*[deal_helper] returns a card list list with the cards in cards
  distributed equally*)
let rec deal_helper
    cards
    num_cards
    acc
    (num_players : int)
    (count : int) =
  if num_players = count then acc
  else
    let player_cards =
      sublist cards (count * num_cards) ((count + 1) * num_cards) [] 0
    in
    let acc = acc @ [ player_cards ] in
    deal_helper cards num_cards acc num_players (count + 1)

(*[deal] deals a deck of cards into [num_players] different card lists
  where each card list has [round_number] of cards, and outputs a trump
  card *)
let deal (deck : deck) num_players round_number =
  let size = Card.get_cards_size deck in
  let num_cards = size / num_players in
  let cards = Card.get_cards deck in
  let trump_card =
    if num_cards * num_players < size then List.nth cards (size - 1)
    else Card.make_no_trump ()
  in
  (deal_helper cards num_cards [] num_players 0, trump_card)
