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
  match read_line () with
  | exception End_of_file -> 0
  | bet -> int_of_string bet

(* This will run the bidding by going through all players Asking for
   their bet *)
let rec run_bidding t_trck bet_sum num_p cntr plyrs =
  if cntr <= num_p then
    match plyrs with
    | hd :: tl ->
        let bet = usr_bet () in
        if bet + bet_sum = t_trck then
          (* Invalid bet *)
          (* ignore (Printf.printf "Bet cannot sum to number of
             tricks!") *)
          run_bidding t_trck bet_sum num_p cntr plyrs
        else
          (* In this case the bet was correct - Assign bet to player -
             Move player to back of queue and ask next player*)
          run_bidding t_trck (bet_sum + bet) num_p cntr
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

(* let compare_cards () *)
let trick (plyrs : Player.t list) (trmp : Card.card) =
  List.map Player.choose_card_rec plyrs
  |> List.map Player.play_card
  |> List.map (fun (x, y) ->
         if Card.get_num y = 15 then Player.win_trick x 
         else x (*THIS IS TEMP*)
         
         )

let play_round (rnd : t) =
  (* Shuffle Deck *)
  match
    Deck.deal (Deck.shuffle rnd.main_deck) rnd.num_players rnd.round_num
  with
  | hands, trump ->
      hands
      (* Assign hands *)
      |> assign_hands rnd.players
      (* We now run bidding. *)
      |> run_bidding rnd.round_num 0 rnd.num_players 0
      (* Now we start game play*)

      (* After round is over prepair for next round *)
      |> gen_next_round rnd
