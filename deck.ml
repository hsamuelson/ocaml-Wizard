open Yojson.Basic.Util

(*[random_compare] randomly returns 1 or -1 as a comparator value*)
let random_compare (a : Card.card) (b : Card.card) =
  Random.self_init ();
  let is_positive = Random.int 2 in
  if is_positive = 1 then 1 else -1

(*[shuffle] sorts [deck]'s cards randomly and returns a shuffled deck*)
let shuffle (deck : Card.card_list) =
  let cards = Card.get_cards deck in
  let shuffled_cards = List.sort random_compare cards in
  Card.set_cards deck shuffled_cards

(*[sublist] returns the sublist of [list] from [index1] inclusive to
  [index2] non-inclusive*)
let rec sublist list index1 index2 acc counter =
  if counter < index1 then sublist list index1 index2 acc index1
  else if counter >= index2 then List.rev acc
  else
    let acc = List.nth list counter :: acc in
    sublist list index1 index2 acc (counter + 1)

(*[deal_helper] returns a card list list with the cards in cards
  distributed equally*)
let rec deal_helper
    cards
    (num_cards : int)
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
  card. Assumes that [num_players] * [round_number] <= size of [deck] *)
let deal (deck : Card.card_list) num_players round_number =
  let size = Card.get_cards_size deck in
  let num_cards = round_number in
  let cards = Card.get_cards deck in
  let trump_card =
    if num_cards * num_players < size then List.nth cards (size - 1)
    else Card.make_no_trump ()
  in
  (deal_helper cards num_cards [] num_players 0, trump_card)

(**[card_of_json] returns a card object based on the given json [j]*)
let card_of_json j : Card.card =
  let number = j |> member "number" |> to_int in
  let suit = j |> member "suit" |> to_string in
  Card.make_card number suit

(**[size_of_json] returns the size of the deck given in the json [j]*)
let size_of_json j : int = j |> member "size" |> to_int

(**[make_deck] returns a deck from the given [json]*)
let make_deck json : Card.card_list =
  let cards =
    json |> member "cards" |> to_list |> List.map card_of_json
  in
  Card.make_card_list cards (size_of_json json)
