open ANSITerminal

type t = {
  main_deck : Card.card_list;
  players : Player.t list;
  num_players : int;
  round_num : int;
}

let init_first_round
    (p_num : int)
    (dck : Card.card_list)
    (plyrs : Player.t list) =
  {
    main_deck = dck;
    players = plyrs;
    num_players = p_num;
    round_num = 1;
  }

(* Change round object to be ready to be run on next round *)
let gen_next_round (rnd : t) (plyrs : Player.t list) =
  { rnd with round_num = rnd.round_num + 1; players = plyrs }

(* This function asks the usr for a bet *)
let usr_bet () =
  print_endline "Enter bet.\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> 0
  | bet -> int_of_string bet

(* A single comment *)
(* This will run the bidding by going through all players Asking for
   their bet *)
let rec run_bidding t_trck bet_sum num_p cntr plyrs =
  if cntr < num_p then
    match plyrs with
    | hd :: tl ->
        (* print_endline (Player.player_to_string hd); *)
        ANSITerminal.erase Screen;
        Player.print_player hd;
        let bet = usr_bet () in
        if bet + bet_sum = t_trck && cntr + 1 = num_p then (
          (*This should only be the case for the last player*)
          (* Invalid bet *)
          (* ignore (Printf.printf "Bet cannot sum to number of
             tricks!") *)
          print_endline "Invalid bet. Please bet again.";
          run_bidding t_trck bet_sum num_p cntr plyrs)
        else
          (* In this case the bet was correct - Assign bet to player -
             Move player to back of queue and ask next player*)
          run_bidding t_trck (bet_sum + bet) num_p (cntr + 1)
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

(**[is_wizard] checks whether the given [player_card] tuple has a wizard
   card*)
let is_wizard (player_card : Player.t * Card.card) : bool =
  if Card.get_num (snd player_card) = 14 then true else false

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
  match player_card_lst with
  | h :: t ->
      if Card.get_suit (snd h) = Card.get_suit trump_card then true
      else exists_trump t trump_card
  | [] -> false

(**[first_trump] returns the first trump card in a list of (player,
   card) tuples. Assumes there is at least one tuple containing a trump
   card*)
let rec first_trump
    (plyr_card : (Player.t * Card.card) list)
    (trump : Card.card) : Player.t * Card.card =
  match plyr_card with
  | h :: t ->
      if Card.get_suit (snd h) = Card.get_suit trump then h
      else first_wizard t
  | [] ->
      failwith
        "precondition violated, need at least one tuple containing a \
         wizard card"

(**[all_zeros] returns true if the given list of (plyer, card) has only
   cards with the number 0, else false*)
let rec all_zeros (player_card_lst : (Player.t * Card.card) list) : bool
    =
  match player_card_lst with
  | h :: t -> if Card.get_num (snd h) <> 0 then false else all_zeros t
  | [] -> true

(**[find_winning_card] takes the given [trump] card and [plyr_card] and
   returns the winning the winning (Player, Card) tuple*)
let find_winning_card
    (trump : Card.card)
    (plyr_card : (Player.t * Card.card) list) =
  (*TODO: factor in first_card_played*)
  let sorted_list = List.sort compare plyr_card in
  if List.exists is_wizard plyr_card then first_wizard plyr_card
    (*return first wizard *)
  else if exists_trump plyr_card trump then first_trump plyr_card trump
  else if all_zeros plyr_card then List.nth plyr_card 0
  else
    match sorted_list with
    | h :: t -> h
    | [] -> failwith "given card list was invalid"

(* Some comparator function *)
let trick (trump : Card.card) (plyrs : Player.t list) =
  List.map Player.choose_card_rec plyrs
  (* |> List.map Player.play_card *)
  |> find_winning_card trump

(* |> List.map (fun (x, y) -> if Card.get_num y = 14 then (*For now
   ignore wizard*) Player.win_trick x

   else x ) *)

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
  print_endline "All Player Bets: ";
  print_endline (bets_to_string bets "" 0);
  ()

let print_list_bets list_players =
  let list_bets = get_list_bets list_players in
  all_bets_to_string list_bets;
  list_players

let rec play_cards_helper list_players acc =
  match list_players with
  | h :: t -> play_cards_helper t (Player.choose_card_rec h :: acc)
  | [] -> acc

let rec update_players_in_list_helper list_players player acc =
  match list_players with
  | h :: t ->
      if Player.player_id player = Player.player_id h then
        acc @ [ player ]
      else update_players_in_list_helper t player acc @ [ h ]
  | [] -> acc

let update_players_in_list list_players player =
  update_players_in_list_helper list_players player []

let play_cards trump list_players =
  let player_card_tuple =
    find_winning_card trump (play_cards_helper list_players [])
  in
  update_players_in_list list_players
    (Player.win_trick (fst player_card_tuple))

let rec finish_players_helper player_list acc =
  match player_list with
  | h :: t -> finish_players_helper t acc @ [ Player.finish_round h ]
  | [] -> acc

let finish_players list_players = finish_players_helper list_players []

let rec play_cards_helper trump list_players round_num =
  if round_num > 0 then
    let new_list_players = play_cards trump list_players in
    play_cards_helper trump new_list_players (round_num - 1)
  else list_players

(**[play_cards_then_finish] should run [play_cards] recursively until
   there are no more cards to play, then it should call
   [Player.finish_round] on each player replace each player in the
   player_list and return that updated player_list*)
let play_cards trump round_num list_players =
  play_cards_helper trump list_players round_num

let print_trump trump player_list : Player.t list =
  print_string [] ("TRUMP CARD: " ^ Card.string_of_card trump ^ "\n \n");
  player_list

let play_round (rnd : t) =
  (* Shuffle Deck *)
  match
    Deck.deal (Deck.shuffle rnd.main_deck) rnd.num_players rnd.round_num
  with
  | hands, trump ->
      hands
      (* Assign hands *)
      |> assign_hands rnd.players
      |> print_trump trump
      (* We now run bidding. *)
      |> run_bidding rnd.round_num 0 rnd.num_players 0
      (* |> trick trump *)
      |> print_list_bets
      (* Now we start game play*)
      |> play_cards trump rnd.round_num
<<<<<<< HEAD
      |> Player.print_player_list
      (*need to have each player select a card and play that card (by
        adding the player card tuples as an input to find_winning_card),
        then take the winning player, augment their score, and have the
        process repeat with the winning player playing first until the
        players are not holding cards, then we have completed one round*)
=======
      |> Player.print_player_list |> finish_players
      |> Player.print_player_list
>>>>>>> 7c26584cd6dd97d0241190646eeaaa11e6620d2d
      (* After round is over prepair for next round *)
      |> gen_next_round rnd

(* let run_all_rounds (rnd : t) (num_players : int) = List.length
   rnd.main_deck mod num_players *)

(* Mabye a function that prints player bets for round *)

let all_bets players = failwith "unimp"
