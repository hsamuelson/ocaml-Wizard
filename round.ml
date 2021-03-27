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

(* This function asks the usr for a bet *)
let usr_bet () = 
  match read_line () with 
  | exception End_of_file -> 0
  | bet -> int_of_string bet 

  (* This will run the bidding by going through all players
  Asking for their bet *)
let rec run_bidding t_trck bet_sum num_p cntr plyrs = 
  if cntr <= num_p then
    match plyrs with
    | hd :: tl -> begin
      let bet = usr_bet () in
      if bet + bet_sum = t_trck then
        (* Invalid bet *)
        (* ignore (Printf.printf "Bet cannot sum to number of tricks!") *)
        run_bidding t_trck bet_sum num_p cntr plyrs
      else
        (* In this case the bet was correct
            - Assign bet to player 
            - Move player to back of queue and ask next player*)
        run_bidding t_trck (bet_sum + bet ) num_p cntr (tl @ [Player.make_bet bet hd])
    end
    (* Not positive that this is an error yet *)
    | _ -> failwith "Error in bidding"
  else plyrs

let rec assign_hands (players : Player.t list) hands =
  match players with
  | hd :: tl -> begin
      match hands with
      | fst_hand :: other_hands ->
          assign_hands (tl @ [Player.give_cards fst_hand hd])
        other_hands
      | _ -> players
      end
  | _ -> failwith "No hands were passed"

let play_round (rnd : t) =
  (* Shuffle Deck *)
  match Deck.shuffle rnd.main_deck |> Deck.deal with
  | (hands, trump) -> hands
  | _ -> []
  (* Assign hands *)
  |> assign_hands rnd.players
  (* We now run bidding. *)
  |> run_bidding rnd.round_num 0 rnd.num_players 0
  (* Now we *)