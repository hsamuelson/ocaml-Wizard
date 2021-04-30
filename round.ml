open ANSITerminal

type t = {
  main_deck : Card.card_list;
  players : Player.t list;
  num_players : int;
  round_num : int;
  calculator : Calculator.t;
}

let players t = t.players

let deck_size t = Card.get_cards_size t.main_deck

let round_num t = t.round_num

let init_first_round
    (p_num : int)
    (dck : Card.card_list)
    (plyrs : Player.t list) =
  {
    main_deck = dck;
    players = plyrs;
    num_players = p_num;
    round_num = 1;
    calculator = Calculator.init dck;
  }

let find_round_leader (plyrs : Player.t list) (round : int) (rnd : t) =
  (* First set round leader to 1 to avoid any errors *)
  (* let rec one_leader (plyrs : Player.t list) = match plyrs with |
     h::t -> begin if (Player.player_id h) = 1 then plyrs else
     one_leader (t @ [h]) end | _ -> failwith "Error" *)
  let compare_sort a b =
    let a1 = Player.player_id a in
    let b2 = Player.player_id b in
    if a1 = b2 then 0 else if a1 > b2 then 1 else -1
  in
  let one_leader (plyrs : Player.t list) =
    List.sort compare_sort plyrs
  in
  (* Mod recursivly until we find the correct leader given round return
     list *)
  let rec find_leader (plyrs : Player.t list) (round : int) counter =
    match plyrs with
    | h :: t ->
        if counter > List.length plyrs then one_leader plyrs
        else if Player.player_id h = round then plyrs
        else find_leader (t @ [ h ]) round (counter + 1)
    | _ -> failwith "Error"
  in
  (* Due to how run_game in table.ml works we need to make sure that
     when this is called we dont initalize a invalid round number *)
  (* if (round) <= ((deck_size rnd) / (rnd.num_players)) then plyrs else *)
  find_leader (one_leader plyrs) (round+1) 1

(* one_leader plyrs *)

(* Change round object to be ready to be run on next round *)
let gen_next_round (rnd : t) (plyrs : Player.t list) =
  {
    (* Increase round number *)
    rnd with
    round_num = rnd.round_num + 1;
    (*Rotate who leads in the next round *)
    (* players = match plyrs with | h::t -> (t @ [h]) | _ -> failwith
       "Error" *)
    players = (find_round_leader plyrs) rnd.round_num rnd;
  }

(* This function asks the usr for a bet *)
let rec usr_bet () =
  print_string
    [ ANSITerminal.green; Bold ]
    "\nEnter bet. [a natural number 0 or larger]]\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> 0
  | bet -> (
      try
        if int_of_string bet >= 0 then int_of_string bet
        else (
          ANSITerminal.print_string
            [ ANSITerminal.red; Bold ]
            "Please enter bet of at least 0";
          usr_bet ())
      with Failure _ ->
        ANSITerminal.print_string
          [ ANSITerminal.red; Bold ]
          "Bet must be a number of at least 0";
        usr_bet ())

let rec robot_bet player trump calc list_cards curr_index acc =
  match list_cards with
  | [] -> acc
  | h :: t ->
      let new_player_card =
        Player.choose_card_at_index player curr_index
      in
      let percentage =
        Player.get_percentage (fst new_player_card)
          (snd new_player_card) calc
      in
      if percentage > 50. then
        robot_bet player trump calc t (curr_index + 1) (acc + 1)
      else robot_bet player trump calc t (curr_index + 1) acc

let print_trump trump player_list : Player.t list =
  ANSITerminal.print_string [ ANSITerminal.white; Bold ] "TRUMP CARD: ";
  Player.print_cards_with_colors_short [ trump ];
  print_endline "\n";
  player_list

let normal_player_bet hd trump t_trck bet_sum num_p cntr plyrs =
  (* print_endline (Player.player_to_string hd); *)
  ANSITerminal.erase Screen;
  print_endline "\n";
  Player.print_player hd;
  print_trump trump [];
  let bet = usr_bet () in
  if bet + bet_sum = t_trck && cntr + 1 = num_p then
    (*This should only be the case for the last player*)
    (* Invalid bet *)
    (* ignore (Printf.printf "Bet cannot sum to number of tricks!") *)
    (bet, true)
  else
    (* In this case the bet was correct - Assign bet to player - Move
       player to back of queue and ask next player*)
    (bet, false)

let robot_player_bet round hd trump t_trck bet_sum num_p cntr plyrs =
  (* print_endline (Player.player_to_string hd); *)
  ANSITerminal.erase Screen;
  print_endline "\n";
  Player.print_player hd;
  print_trump trump [];
  ANSITerminal.print_string
    [ ANSITerminal.green; Bold ]
    "Robot making bet... press enter.";
  match read_line () with
  | _ ->
      let bet =
        robot_bet hd trump round.calculator
          (Player.get_player_hand_list hd)
          0 0
      in
      if bet + bet_sum = t_trck && cntr + 1 = num_p then
        (*This should only be the case for the last player*)
        (* Invalid bet *)
        (* ignore (Printf.printf "Bet cannot sum to number of tricks!") *)
        (bet, true)
      else
        (* In this case the bet was correct - Assign bet to player -
           Move player to back of queue and ask next player*)
        (bet, false)

(* A single comment *)
(* This will run the bidding by going through all players Asking for
   their bet *)
let rec run_bidding round trump t_trck bet_sum num_p cntr plyrs =
  if cntr < num_p then
    match plyrs with
    | hd :: tl ->
        (* print_endline (Player.player_to_string hd); *)
        if not (Player.get_is_robot hd) then
          let output =
            normal_player_bet hd trump t_trck bet_sum num_p cntr plyrs
          in
          let bet = fst output in
          if snd output then (
            (*This should only be the case for the last player*)
            (* Invalid bet *)
            (* ignore (Printf.printf "Bet cannot sum to number of
               tricks!") *)
            print_endline "Invalid bet. Please bet again.";
            run_bidding round trump t_trck bet_sum num_p cntr plyrs)
          else
            (* In this case the bet was correct - Assign bet to player -
               Move player to back of queue and ask next player*)
            run_bidding round trump t_trck (bet_sum + bet) num_p
              (cntr + 1)
              (tl @ [ Player.make_bet bet hd ])
        else
          let robot_output =
            robot_player_bet round hd trump t_trck bet_sum num_p cntr
              plyrs
          in
          let bet = fst robot_output in
          if snd robot_output then
            (*This should only be the case for the last player*)
            (* Invalid bet *)
            (* ignore (Printf.printf "Bet cannot sum to number of
               tricks!") *)
            let new_bet = bet + 1 in
            run_bidding round trump t_trck (bet_sum + new_bet) num_p
              (cntr + 1)
              (tl @ [ Player.make_bet new_bet hd ])
          else
            (* In this case the bet was correct - Assign bet to player -
               Move player to back of queue and ask next player*)
            run_bidding round trump t_trck (bet_sum + bet) num_p
              (cntr + 1)
              (tl @ [ Player.make_bet bet hd ])
    (* Not positive that this is an error yet *)
    | _ -> failwith "Error in bidding"
  else plyrs

let rec assign_hands (players : Player.t list) hands =
  match players with
  | hd :: tl -> (
      match hands with
      | fst_hand :: other_hands ->
          assign_hands
            (tl @ [ Player.give_cards fst_hand hd ])
            other_hands
      | _ -> players)
  | _ -> failwith "No hands were passed"

(* A longer version of trick *)
(* let rec trick (rnd : t) (cntr : int) (plyrs : Player.t list) :
   Player.t list = if cntr <= rnd.num_players then match plyrs with | hd
   :: tl -> begin trick rnd (cntr + 1) ( tl @ [Player.choose_card_rec
   hd]) end | _ -> plyrs else plyrs *)

(**[exists_wizard] checks whether the given [player_card_lst] has a
   wizard card in one of its tuples*)
let rec exists_wizard (player_card_lst : (Player.t * Card.card) list) :
    bool =
  List.map (fun (x, y) -> Card.get_num y = 14) player_card_lst
  |> List.fold_left ( || ) false

(**[first_wizard] returns the first wizard card in a list of (player,
   card) tuples.Card. Assumes there is at least one tuple containing a
   wizard card*)
let rec first_wizard (plyr_card : (Player.t * Card.card) list) :
    Player.t * Card.card =
  match plyr_card with
  | h :: t -> if Card.get_num (snd h) = 14 then h else first_wizard t
  | [] ->
      failwith
        "precondition violated, need at least one tuple containing a \
         wizard card"

(**[exists_trump] checks whether the given [player_card_lst] has a trump
   suit card in one of its tuples*)
let rec exists_trump
    (player_card_lst : (Player.t * Card.card) list)
    (trump_card : Card.card) : bool =
  List.map
    (fun (x, y) ->
      if
        Card.get_suit y = Card.get_suit trump_card && Card.get_num y > 0
      then true
      else false)
    player_card_lst
  |> List.fold_left ( || ) false

(**[first_trump] returns the first trump card in a list of (player,
   card) tuples. Assumes there is at least one tuple containing a trump
   card*)
let rec first_trump
    (plyr_card : (Player.t * Card.card) list)
    (trump : Card.card) : Player.t * Card.card =
  match plyr_card with
  | h :: t ->
      if
        Card.get_suit (snd h) = Card.get_suit trump
        && Card.get_num (snd h) > 0
      then h
      else first_trump t trump
  | [] ->
      failwith
        "precondition violated, need at least one tuple containing a \
         trump card"

(**[all_zeros] returns true if the given list of (plyer, card) has only
   cards with the number 0, else false*)
let rec all_zeros (player_card_lst : (Player.t * Card.card) list) : bool
    =
  match player_card_lst with
  | h :: t -> if Card.get_num (snd h) <> 0 then false else all_zeros t
  | [] -> true

(** [compare_player_card_tuples] is a comparator function to sort a list
    of cards in descending order according to their card numbers*)
let compare_player_card_tuples t1 t2 =
  if Card.get_num (snd t1) > Card.get_num (snd t2) then -1
  else if Card.get_num (snd t1) < Card.get_num (snd t2) then 1
  else 0

let rec find_first_nonzero_card tuple_list =
  match tuple_list with
  | h :: t ->
      if Card.get_num (snd h) > 0 then h else find_first_nonzero_card t
  | [] ->
      failwith "impossible to fail, somehow did not find non-zero card"

(**[find_winning_card] takes the given [trump] card and [plyr_card] and
   returns the winning the winning (Player, Card) tuple*)
let find_winning_card
    (trump : Card.card)
    (plyr_card : (Player.t * Card.card) list) =
  let plyr_card_right_order = List.rev plyr_card in
  let sorted_list =
    List.sort compare_player_card_tuples plyr_card_right_order
  in
  if exists_wizard plyr_card_right_order then
    first_wizard plyr_card_right_order
  else if exists_trump sorted_list trump then
    first_trump sorted_list trump
  else if all_zeros plyr_card_right_order then
    List.nth plyr_card_right_order 0
  else
    (*find first non-zero card, treat it like a trump card*)
    let secondary_trump =
      find_first_nonzero_card plyr_card_right_order
    in
    first_trump sorted_list (snd secondary_trump)

let rec get_list_bets list_players =
  match list_players with
  | [] -> []
  | h :: t -> Player.player_bet h :: get_list_bets t

let rec bets_to_string bets acc indx =
  match bets with
  | [] -> acc
  | h :: t ->
      bets_to_string t
        (acc ^ "[Player " ^ string_of_int indx ^ " bet "
       ^ string_of_int h ^ "] ")
        (indx + 1)

let all_bets_to_string (bets : int list) =
  ANSITerminal.erase Screen;
  print_endline "\nAll Player Bets: ";
  print_endline (bets_to_string bets "" 0);
  print_endline "\n\n"

let print_list_bets list_players =
  let list_bets = get_list_bets list_players in
  all_bets_to_string list_bets;
  list_players

let rec player_plays_card round trump list_players acc =
  ANSITerminal.erase Screen;
  print_endline "\n";
  print_trump trump [];
  ANSITerminal.print_string
    [ ANSITerminal.white; Bold ]
    "PLAYED CARDS: ";
  let played_cards = List.rev (List.map snd acc) in
  Player.print_cards_with_colors_short played_cards;
  print_endline "\n\n";
  match list_players with
  | h :: t ->
      let played_cards = List.map snd acc in
      let calc = round.calculator in
      let new_played_card =
        Player.choose_card_rec played_cards calc trump h played_cards
      in
      let new_round =
        {
          round with
          calculator =
            Calculator.update_unplayed round.calculator
              (snd new_played_card);
        }
      in
      player_plays_card new_round trump t (new_played_card :: acc)
  | [] -> acc

let rec update_players_in_list_helper list_players player acc =
  match list_players with
  | h :: t ->
      if Player.player_id player = Player.player_id h then
        update_players_in_list_helper t player acc @ [ player ]
      else update_players_in_list_helper t player acc @ [ h ]
  | [] -> acc

let update_players_in_list list_players player =
  update_players_in_list_helper list_players player []

let print_winner winner_tuple player_tuples =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string
    [ ANSITerminal.white; Bold ]
    "PLAYED CARDS: ";
  Player.print_cards_with_colors_short (List.map snd player_tuples);
  print_endline "\n";

  ANSITerminal.print_string
    [ ANSITerminal.white; Bold ]
    "WINNING CARD: ";
  Player.print_cards_with_colors_short [ snd winner_tuple ];
  print_endline "\n";
  ANSITerminal.print_string [ ANSITerminal.white; Bold ] "WINNER: ";
  ANSITerminal.print_string
    [ ANSITerminal.magenta; Bold ]
    ("Player " ^ string_of_int (Player.player_id (fst winner_tuple)));
  print_endline "\n\n";
  match read_line () with exception End_of_file -> () | _ -> ()

let play_card round trump list_players =
  let players_played = player_plays_card round trump list_players [] in
  let updated_players = List.map fst players_played in
  let player_card_tuple = find_winning_card trump players_played in
  print_winner player_card_tuple players_played;
  update_players_in_list updated_players
    (Player.win_trick (fst player_card_tuple))

let rec finish_players_helper player_list acc =
  match player_list with
  | h :: t -> finish_players_helper t acc @ [ Player.finish_round h ]
  | [] -> acc

let finish_players list_players = finish_players_helper list_players []

let rec play_cards_helper round trump list_players round_num =
  if round_num > 0 then
    let new_list_players = play_card round trump list_players in
    play_cards_helper round trump new_list_players (round_num - 1)
  else list_players

(**[play_cards] should run [play_cards] recursively until there are no
   more cards to play*)
let play_cards round trump round_num list_players =
  play_cards_helper round trump list_players round_num

let rec list_to_string acc lst =
  match lst with
  | [] -> acc
  | hd :: tl -> list_to_string (acc ^ " " ^ string_of_int hd) tl

let scoreboard (p_list : Player.t list) =
  let scores =
    p_list
    |> List.map (fun x -> Player.player_score x)
    (* |> List.map string_of_int *)
    |> list_to_string ""
  in
  let ids =
    p_list
    |> List.map (fun x -> Player.player_id x)
    (* |> List.map string_of_int *)
    |> list_to_string ""
  in
  (ids, scores)

let print_scoreboard rnd : unit =
  let score_b = scoreboard rnd.players in
  (* Round number print *)
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    ("\nRound " ^ string_of_int rnd.round_num ^ "\n");
  ANSITerminal.print_string
    [ ANSITerminal.red; Bold ]
    "\n\
    \             ▄▄▄███ SCOREBOARD ███▄▄▄   \n";
  print_string
    [ ANSITerminal.red; Bold ]
    "▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄\n";

  print_string
    [ ANSITerminal.red; Bold ]
    "█           █                            \n";
  print_string
    [ ANSITerminal.red; Bold ]
    ("█ Player ID █ " ^ fst score_b ^ "        \n");
  print_string
    [ ANSITerminal.red; Bold ]
    "█▄▄▄▄▄▄▄▄▄▄▄█▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄\n";
  print_string
    [ ANSITerminal.red; Bold ]
    "█           █                            \n";
  print_string
    [ ANSITerminal.red; Bold ]
    ("█ Score     █ " ^ snd score_b ^ "        \n");
  print_string
    [ ANSITerminal.red; Bold ]
    "█           █                            \n";
  print_string
    [ ANSITerminal.red; Bold ]
    "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ \n";
  print_string [] "\n\nPress enter to continue...\n";
  match read_line () with _ -> ()

let play_round (rnd : t) =
  (* Print round number *)
  (* Print scoreboard *)
  print_scoreboard rnd;

  (* Shuffle Deck *)
  match
    Deck.deal (Deck.shuffle rnd.main_deck) rnd.num_players rnd.round_num
  with
  | hands, trump ->
      hands
      (* Assign hands *)
      |> assign_hands rnd.players
      (* We now run bidding. *)
      |> run_bidding rnd trump rnd.round_num 0 rnd.num_players 0
      (* |> trick trump *)
      |> print_list_bets
      (* Now we start game play*)
      |> play_cards rnd trump rnd.round_num
      |> finish_players
      (* After round is over prepair for next round *)
      (* |> Player.print_player_list *)
      |> List.map Player.reset_round_player
      |> gen_next_round rnd

(* let run_all_rounds (rnd : t) (num_players : int) = List.length
   rnd.main_deck mod num_players *)

(* Mabye a function that prints player bets for round *)

let all_bets players = failwith "unimp"
