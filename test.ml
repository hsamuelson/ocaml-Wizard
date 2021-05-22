(**TEST PLAN: Our test system automatically tests the fundamental
   functions for our wizard game with OUnit tests. These include the
   tests of making bets, initializing the deck, shuffling the deck,
   assigning cards, finishing and resetting the round, and more. The
   more complex functions, which only make subtle changes to the game
   state, were tested manually by comparing the gameplay of our wizard
   game with the expected gameplay mechanics as observed by veteran
   wizard players with a ruleset handy. The ruleset, which is provided
   in rules.txt and can be read by typing in 'rules' when prompted,
   proved quite useful to manually test the edge cases in card
   interactions, as there are many. We test the Player, Deck, Card,
   Table, Calculator, and Round modules, leaving only the Main and
   PrintFunct modules untested by OUnit. We developed our tests using
   black-box testing and glass-box testing where appropriate, as
   sometimes we knew there was an aspect to our implementation that we
   needed to test, such as edge cases with empty lists, hence we would
   use glass-box-testing, and sometimes we needed to check for the
   correctness of the output because it was not obvious how the output
   might be correct or incorrect based on the code alone, hence we used
   black-box testing to design those tests. We performed manual
   randomized testing for the functions in the Main module, as those
   functions often take keyboard input as inputs, meaning they need to
   be thoroughly protected from crashing on fuzzy inputs, which our
   manual testing proved they were. We manually tested every single
   input with randomized testing until it was impossible to break the
   code with a bad input. The reason this test plan demonstrates the
   correctness of the system is because the fundamental aspects of the
   system are tested modularly. These aspects which are correct in their
   modular setting are then combined in the manual tests, in which we
   carefully check for any conditions that might cause functions which
   pass their tests in modularity to fail in combinatio with others
   functions.*)

open OUnit2
open Player
open Deck
open Card
open Round
open Table

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

(* To test that the deck is shuffled, we checked that at least half of
   the deck elements shift position, which is nearly guaranteed by
   reshuffling a deck of cards of any significant size *)
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
    (Player.player_to_list (Player.initialize_player id false))
    ~printer:(print_player_list "[ ")

let initialize_tests =
  [
    initialize_test "Initialize player 1" 1 [ 0; 0; 0; 0; 0; 0; 1 ];
    initialize_test "Initialize player 2" 2 [ 0; 0; 0; 0; 0; 0; 2 ];
  ]

let player1 = Player.initialize_player 1 false

let player2 = Player.initialize_player 2 false

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

let player3 = Player.initialize_player 3 false

let player4 = Player.initialize_player 4 false

let player5 = Player.initialize_player 5 false |> Player.make_bet 2

let player2 = Player.initialize_player 2 false

let player1 = Player.initialize_player 1 false

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
    win_trick_test "Player 1 win trick" player1 [ 0; 1; 0; 0; 0; 0; 1 ];
    win_trick_test "Player 2 win trick" player2 [ 0; 1; 0; 0; 0; 0; 2 ];
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
  Player.initialize_player 6 false
  |> Player.make_bet 2 |> Player.win_trick

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
  Player.initialize_player 7 false
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4

let player8 =
  Player.initialize_player 8 false
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4
  |> Player.choose_card "next"

let player9 =
  Player.initialize_player 9 false
  |> Player.make_bet 2 |> Player.win_trick
  |> Player.give_cards card_hand_4
  |> Player.choose_card "next"
  |> Player.choose_card "next"

let player10 =
  Player.initialize_player 10 false
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

let player12 =
  Player.initialize_player 12 false
  |> Player.make_bet 2 |> Player.win_trick |> Player.win_trick

let player13 =
  Player.initialize_player 13 false
  |> Player.make_bet 2 |> Player.win_trick

let player14 = Player.initialize_player 14 false |> Player.make_bet 0

let player15 =
  Player.initialize_player 15 false
  |> Player.make_bet 0 |> Player.win_trick

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
  Player.initialize_player 16 false
  |> Player.make_bet 2 |> Player.win_trick |> Player.win_trick
  |> Player.finish_round

let player17 =
  Player.initialize_player 17 false
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
    finish_round_tests;
    reset_round_tests;
  ]

let card_hand =
  make_card_list [ Card.make_card 1 "red"; Card.make_card 2 "orange" ] 2

let empty_card_hand = make_card_list [] 0

let init_calculator_test
    (name : string)
    (card_list : Card.card_list)
    (expected_output : Card.card_list) =
  name >:: fun _ ->
  assert_equal expected_output
    (Calculator.get_unplayed (Calculator.init card_list))
    ~printer:card_list_printer

let init_calculator_tests =
  [
    init_calculator_test "intialize empty calc" card_hand card_hand;
    init_calculator_test "initialize non-empty calc" empty_card_hand
      empty_card_hand;
  ]

let card_hand2 =
  make_card_list [ Card.make_card 1 "red"; Card.make_card 2 "orange" ] 2

let calc1 = Calculator.init card_hand2

let card1 = Card.make_card 1 "red"

let first_removed = make_card_list [ Card.make_card 2 "orange" ] 2

let card2 = Card.make_card 2 "orange"

let second_removed = make_card_list [ Card.make_card 1 "red" ] 2

let string_of_string s = s

let update_unplayed_test
    (name : string)
    (calc : Calculator.t)
    (card : Card.card)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (card_list_printer
       (Calculator.get_unplayed (Calculator.update_unplayed calc card)))
    ~printer:string_of_string

let update_unplayed_tests =
  [
    update_unplayed_test "remove first card" calc1 card1
      "[ 2 , orange ]";
    update_unplayed_test "remove last card" calc1 card2 "[ 1 , red ]";
  ]

let calculator_tests = [ init_calculator_tests; update_unplayed_tests ]

(*deck testing ends here*)
(* Round testing starts here *)
let playrList = [ player6; player7; player8 ]

let plarys2 = [ player7; player8; player9 ]

let round1 = init_first_round 3 main_deck playrList

let round2 = gen_next_round round1 playrList

let round_gen_tests =
  [
    ( "Check playr list is correct " >:: fun _ ->
      assert_equal playrList (players round1) );
    ("Check round num" >:: fun _ -> assert_equal 1 (round_num round1));
    ("Check deck size " >:: fun _ -> assert_equal 60 (deck_size round1));
  ]

let round_find_leader_tests =
  [
    ( "Check correct leader " >:: fun _ ->
      assert_equal playrList (find_round_leader playrList 1 round1) );
    ( "Check correct leader of second round " >:: fun _ ->
      assert_equal playrList
        (find_round_leader [ player7; player8; player6 ] 2 round1) );
    ( "Check correct leader is still correct when looped around table "
    >:: fun _ ->
      assert_equal playrList
        (find_round_leader [ player6; player7; player8 ] 4 round1) );
  ]

let round_gen_next_round_tests =
  [
    ( "Check round num is updated " >:: fun _ ->
      assert_equal 2 (round_num round2) );
    ( "Check player order is updated " >:: fun _ ->
      assert_equal [ player6; player7; player8 ] (players round2) );
  ]

let score_board_tests =
  [
    ( "Check scoreboard correctly runs 3 players" >:: fun _ ->
      assert_equal (" 1 2 3", " 0 0 0") (scoreboard (init_players 3 0))
    );
    ( "Check scorboard correctly runs 2 players " >:: fun _ ->
      assert_equal (" 1 2", " 0 0") (scoreboard (init_players 2 0)) );
    ( "Check scorboard with robots and humans " >:: fun _ ->
      assert_equal
        (" 1 2 3 4 5", " 0 0 0 0 0")
        (scoreboard (init_players 3 2)) );
  ]

let card3 = Card.make_card 10 "red"

let card4 = Card.make_card 11 "red"

let card5 = Card.make_card 4 "blue"

let card6 = Card.make_card 5 "blue"

let card7 = Card.make_card 8 "red"

let card8_wizard = Card.make_card 14 "green"

let playr_card_list =
  [ (player7, card4); (player8, card3); (player9, card1) ]

let playr_card_list2 =
  [ (player7, card4); (player8, card3); (player9, card5) ]

let playr_card_list3 =
  [ (player7, card4); (player8, card3); (player9, card5) ]

let playr_card_list4 =
  [ (player7, card4); (player8, card8_wizard); (player9, card5) ]

let find_winning_card_tests =
  [
    ( "Win correctly with only one suite thats not trump" >:: fun _ ->
      assert_equal (player7, card4)
        (find_winning_card card5 playr_card_list) );
    ( "Win correctly with all trump" >:: fun _ ->
      assert_equal (player7, card4)
        (find_winning_card card7 playr_card_list) );
    ( "Win correctly with low trump" >:: fun _ ->
      assert_equal (player9, card5)
        (find_winning_card card6 playr_card_list2) );
    ( "Win correctly with muiltple trump and non-trump" >:: fun _ ->
      assert_equal (player7, card4)
        (find_winning_card card1 playr_card_list3) );
    ( "Win with non trump /suite wizard card" >:: fun _ ->
      assert_equal (player8, card8_wizard)
        (find_winning_card card1 playr_card_list4) );
  ]

let round_tests =
  [
    round_gen_tests;
    round_find_leader_tests;
    round_gen_next_round_tests;
    score_board_tests;
    find_winning_card_tests;
  ]

let table_tests =
  [ (* ("Initilize plar list with no robots " >:: fun _ -> assert_equal
       playrList (init_players 3 0) ); *) ]

let suite =
  "test suite for Wizard"
  >::: List.flatten
         [
           shuffle_tests;
           deal_tests;
           List.flatten player_tests;
           List.flatten calculator_tests;
           List.flatten round_tests;
           table_tests;
         ]

let _ = run_test_tt_main suite
