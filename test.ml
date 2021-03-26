(*Main file in which we call the module test files from *)
open OUnit2

(*Note: make test will fail if you uncomment modules that output
  failwith for any function *)

(* open Table *)
open Deck
open Card
(* open Round open Player *)

(*create deck for testing from json file*)
let j = Yojson.Basic.from_file "main_deck.json"

(*deck testing begins here*)
let main_deck = make_deck j

let rec deck_not_equal_helper cards1 cards2 acc =
  match cards1 with
  | [] -> acc
  | h1 :: t1 -> (
      match cards2 with
      | [] -> -1
      | h2 :: t2 ->
          if h1 = h2 then deck_not_equal_helper t1 t2 acc
          else deck_not_equal_helper t1 t2 (acc + 1))

(* I choose to check whether half the elts in the original deck shift to
   a new spot in the new deck *)
let cmp_decks_not_equal deck1 deck2 =
  let cards1 = get_cards deck1 in
  let cards2 = get_cards deck2 in
  let num_non_equal_elts = deck_not_equal_helper cards1 cards2 0 in
  if Card.get_cards_size deck1 / num_non_equal_elts < 2 then true
  else false

let card_concatenator acc card =
  let card_string = Card.string_of_card card in
  acc ^ card_string

let card_list_printer (card_list : card_list) : string =
  let cards = get_cards card_list in
  List.fold_left card_concatenator "" cards

let shuffle_test
    (name : string)
    (deck : card_list)
    (expected_output : card_list) : test =
  name >:: fun _ ->
  assert_equal expected_output (shuffle deck) ~cmp:cmp_decks_not_equal
    ~printer:card_list_printer ~msg:"FAILED: decks are the same"

let shuffle_tests =
  [ shuffle_test "shuffle basic wizard deck" main_deck main_deck ]

let list_of_list_of_strings_of_deck_list lst =
  List.map (List.map string_of_card) lst

(*for each list of lists, create a list of card strings, then flatten
  the list*)
let string_of_deck_deal tuple =
  let card_list_list = fst tuple in
  card_list_list |> list_of_list_of_strings_of_deck_list |> List.flatten

let string_list_printer (string_lst : string list) : string =
  List.fold_left ( ^ ) "" string_lst

let string_of_string_lst lst1 lst2 =
  let string1 = string_list_printer lst1 in
  let string2 = string_list_printer lst2 in
  if string1 = string2 then true else false

(**[deal_test] checks that there are [num_players] card lists of size
   (size/ num_players)*)
let deal_test
    (name : string)
    (deck : card_list)
    (num_players : int)
    (round_number : int)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal expected_output
    (string_of_deck_deal (deal deck num_players round_number))
    ~cmp:string_of_string_lst ~printer:string_list_printer

let full_deck_list =
  [
    "[ 0 , red ][ 1 , red ][ 2 , red ][ 3 , red ][ 4 , red ][ 5 , red \
     ][ 6 , red ][ 7 , red ][ 8 , red ][ 9 , red ][ 10 , red ][ 11 , \
     red ][ 12 , red ][ 13 , red ][ 14 , red ][ 0 , red ][ 1 , yellow \
     ][ 2 , yellow ][ 3 , yellow ][ 4 , yellow ][ 5 , yellow ][ 6 , \
     yellow ][ 7 , yellow ][ 8 , yellow ][ 9 , yellow ][ 10 , yellow \
     ][ 11 , yellow ][ 12 , yellow ][ 13 , yellow ][ 14 , yellow ][ 0 \
     , blue ][ 1 , blue ][ 2 , blue ][ 3 , blue ][ 4 , blue ][ 5 , \
     blue ][ 6 , blue ][ 7 , blue ][ 8 , blue ][ 9 , blue ][ 10 , blue \
     ][ 11 , blue ][ 12 , blue ][ 13 , blue ][ 14 , blue ][ 0 , green \
     ][ 1 , green ][ 2 , green ][ 3 , green ][ 4 , green ][ 5 , green \
     ][ 6 , green ][ 7 , green ][ 8 , green ][ 9 , green ][ 10 , green \
     ][ 11 , green ][ 12 , green ][ 13 , green ][ 14 , green ]";
  ]

let deal_tests =
  [
    deal_test "deal unshuffled wizard deck, 1 player round 1" main_deck
      1 1 [ "[ 0 , red ]" ];
    deal_test "deal unshuffled wizard deck, 2 players round 1" main_deck
      2 1
      [ "[ 0 , red ]"; "[ 1 , red ]" ];
    deal_test "deal unshuffled wizard deck, 2 players round 2" main_deck
      2 2
      [ "[ 0 , red ]"; "[ 1 , red ]"; "[ 2 , red ]"; "[ 3 , red ]" ];
    deal_test "deal unshuffled wizard deck, 4 players round 15"
      main_deck 4 15 full_deck_list;
    deal_test "deal unshuffled wizard deck, 6 players round 10"
      main_deck 6 10 full_deck_list;
  ]

(*deck testing ends here*)
let suite =
  "test suite for Wizard"
  >::: List.flatten [ shuffle_tests; deal_tests ]

let _ = run_test_tt_main suite
