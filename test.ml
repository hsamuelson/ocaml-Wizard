(*Main file in which we call the module test files from *)
open OUnit2
open Player

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

let deal_tests =
  [
    deal_test "deal unshuffled wizard deck, 1 player round 1" main_deck
      1 1 [ "hello" ];
  ]

let rec print_player_list str player_list =
  match player_list with
  | [] -> str ^ " ]"
  | h :: t -> print_player_list (str ^ " " ^ string_of_int h ^ ",") t

let initialize_test
    (name : string)
    (id : int)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.player_to_list (Player.initialize_player id))
    ~printer:(print_player_list "[ ")

let initialize_tests =
  [
    initialize_test "Initialize player 1" 1 [ 0; 0; 0; 0; 0; 0; 1 ];
    initialize_test "Initialize player 2" 2 [ 0; 0; 0; 0; 0; 0; 2 ];
  ]

let player1 = Player.initialize_player 1

let player2 = Player.initialize_player 2

let make_bet_test
    (name : string)
    (player : Player.t)
    (bet : int)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.player_to_list (Player.make_bet bet player))
    ~printer:(print_player_list "[ ")

let make_bet_tests =
  [
    make_bet_test "Player 1 make bet of 0" player1 0
      [ 0; 0; 0; 0; 0; 0; 1 ];
    make_bet_test "Player 2 make bet of 1" player2 1
      [ 1; 0; 0; 0; 0; 0; 2 ];
  ]

let player_tests =
  [
    initialize_tests;
    make_bet_tests;
    (* win_trick_tests; *)
    (* reset_round_tests; *)
    (*choose_card_tests; play_card_tests; finish_round_tests; *)
  ]

(*deck testing ends here*)
let suite =
  "test suite for Wizard"
  >::: List.flatten [ shuffle_tests; List.flatten player_tests ]

let _ = run_test_tt_main suite
