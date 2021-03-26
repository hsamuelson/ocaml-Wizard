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
    (Player.make_bet bet player |> Player.player_to_list)
    ~printer:(print_player_list "[ ")

let make_bet_tests =
  [
    make_bet_test "Player 1 make bet of 0" player1 0
      [ 0; 0; 0; 0; 0; 0; 1 ];
    make_bet_test "Player 2 make bet of 1" player2 1
      [ 1; 0; 0; 0; 0; 0; 2 ];
    make_bet_test "Player 2 make bet of 3" player2 3
      [ 3; 0; 0; 0; 0; 0; 2 ];
  ]

let player3 = Player.initialize_player 3

let player4 = Player.initialize_player 4

let player5 = Player.initialize_player 5 |> Player.make_bet 2

let win_trick_test
    (name : string)
    (player : Player.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.win_trick player |> Player.player_to_list)
    ~printer:(print_player_list "[ ")

let win_trick_tests =
  [
    win_trick_test "Player 3 win trick" player3 [ 0; 1; 0; 0; 0; 0; 3 ];
    win_trick_test "Player 4 win trick" player4 [ 0; 1; 0; 0; 0; 0; 4 ];
    win_trick_test "Player 5 win trick" player5 [ 2; 1; 0; 0; 0; 0; 5 ];
  ]

let card_hand_2 = [ Card.make_card 1 "red"; Card.make_card 2 "orange" ]

let card_hand_4 =
  [
    Card.make_card 1 "red";
    Card.make_card 2 "orange";
    Card.make_card 3 "yellow";
    Card.make_card 4 "green";
  ]

let player6 =
  Player.initialize_player 6 |> Player.make_bet 2 |> Player.win_trick

let give_cards_test
    (name : string)
    (player : Player.t)
    (cards : Card.card list)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.give_cards cards player |> Player.player_to_list)
    ~printer:(print_player_list "[ ")

let give_cards_tests =
  [
    give_cards_test "Player 6 give empty deck" player6 []
      [ 2; 1; 0; 0; 0; 0; 6 ];
    give_cards_test "Player 6 give 2 cards" player6 card_hand_2
      [ 2; 1; 0; 2; 1; 0; 6 ];
    give_cards_test "Player 6 give 4 cards" player6 card_hand_4
      [ 2; 1; 0; 4; 1; 0; 6 ];
  ]

let player7 =
  Player.initialize_player 7
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4

let player8 =
  Player.initialize_player 8
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4
  |> Player.choose_card "next"

let player9 =
  Player.initialize_player 9
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4
  |> Player.choose_card "next"
  |> Player.choose_card "next"

let player10 =
  Player.initialize_player 10
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4
  |> Player.choose_card "next"

let print_string str = str

let get_cards_test
    (name : string)
    (player : Player.t)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.get_player_hand player)
    ~printer:print_string

let get_cards_tests =
  [
    get_cards_test "Player 7 get hand" player7
      "[ 1 , red ][ 2 , orange ][ 3 , yellow ][ 4 , green ]";
  ]

let choose_card_test
    (name : string)
    (player : Player.t)
    (movement : string)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.choose_card movement player |> Player.player_to_list)
    ~printer:(print_player_list "[ ")

let choose_card_tests =
  [
    choose_card_test "Player 7 chose next" player7 "next"
      [ 2; 1; 0; 4; 2; 1; 7 ];
    choose_card_test "Player 8 choose next" player8 "next"
      [ 2; 1; 0; 4; 3; 2; 8 ];
    choose_card_test "Player 9 choose next" player9 "next"
      [ 2; 1; 0; 4; 4; 3; 9 ];
    choose_card_test "Player 10 choose prev" player10 "prev"
      [ 2; 1; 0; 4; 1; 0; 10 ];
  ]

let play_card_test_player
    (name : string)
    (player : Player.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (let pl =
       match Player.play_card player with
       | fst, lst -> Player.player_to_list fst
     in
     pl)
    ~printer:(print_player_list "[ ")

let play_card_tests_get_player =
  [
    play_card_test_player "Player 7 plays first [idx 0] card" player7
      [ 2; 1; 0; 3; 2; 0; 7 ];
    play_card_test_player "Player 8 plays second [idx 1] card" player8
      [ 2; 1; 0; 3; 2; 0; 8 ];
    play_card_test_player "Player 9 plays third [idx 2] card" player9
      [ 2; 1; 0; 3; 3; 1; 9 ];
    play_card_test_player "Player 10 plays third [idx 1] card" player10
      [ 2; 1; 0; 3; 2; 0; 10 ];
  ]

let play_card_test_card
    (name : string)
    (player : Player.t)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (let pl =
       match Player.play_card player with
       | fst, lst -> Card.string_of_card lst
     in
     pl)
    ~printer:print_string

let play_card_tests_get_card =
  [
    play_card_test_card "Player 7 plays first [idx 0] card" player7
      "[ 1 , red ]";
    play_card_test_card "Player 8 plays second [idx 1] card" player8
      "[ 2 , orange ]";
    play_card_test_card "Player 9 plays third [idx 2] card" player9
      "[ 3 , yellow ]";
    play_card_test_card "Player 10 plays third [idx 2] card" player10
      "[ 2 , orange ]";
  ]

let player12 =
  Player.initialize_player 12
  |> Player.make_bet 2 |> Player.win_trick |> Player.win_trick

let player13 =
  Player.initialize_player 13 |> Player.make_bet 2 |> Player.win_trick

let player14 = Player.initialize_player 14 |> Player.make_bet 0

let player15 =
  Player.initialize_player 15 |> Player.make_bet 0 |> Player.win_trick

let finish_round_test
    (name : string)
    (player : Player.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.finish_round player |> Player.player_to_list)
    ~printer:(print_player_list "[ ")

let finish_round_tests =
  [
    finish_round_test "Player 12 bet == wins" player12
      [ 2; 2; 3; 0; 0; 0; 12 ];
    finish_round_test "Player 13 bet != wins" player13
      [ 2; 1; -1; 0; 0; 0; 13 ];
    finish_round_test "Player 14 bet == 0 and bet == wins" player14
      [ 0; 0; 1; 0; 0; 0; 14 ];
    finish_round_test "Player 15 bet == 0 and bet == 0 and wins == 1"
      player15
      [ 0; 1; -1; 0; 0; 0; 15 ];
  ]

let player16 =
  Player.initialize_player 16
  |> Player.make_bet 2 |> Player.win_trick |> Player.win_trick
  |> Player.finish_round

let player17 =
  Player.initialize_player 17
  |> Player.make_bet 3 |> Player.win_trick |> Player.win_trick
  |> Player.win_trick
  |> Player.give_cards card_hand_4
  |> Player.choose_card "next"
  |> Player.choose_card "next"
  |> Player.finish_round

let reset_round_test
    (name : string)
    (player : Player.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.reset_round_player player |> Player.player_to_list)
    ~printer:(print_player_list "[ ")

let reset_round_tests =
  [
    reset_round_test "Player 16 reset" player16 [ 0; 0; 3; 0; 0; 0; 16 ];
    reset_round_test "Player 17 reset" player17 [ 0; 0; 4; 0; 0; 0; 17 ];
  ]

let player_tests =
  [
    initialize_tests;
    make_bet_tests;
    win_trick_tests;
    give_cards_tests;
    get_cards_tests;
    choose_card_tests;
    play_card_tests_get_player;
    play_card_tests_get_card;
    finish_round_tests;
    reset_round_tests;
  ]

(*deck testing ends here*)
let suite =
  "test suite for Wizard"
  >::: List.flatten
         [ shuffle_tests; deal_tests; List.flatten player_tests ]

let _ = run_test_tt_main suite
