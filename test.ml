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

(*deck testing ends here*)
let suite = "test suite for Wizard" >::: List.flatten [ shuffle_tests ]

let _ = run_test_tt_main suite
