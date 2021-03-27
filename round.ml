type t = {
  main_deck : Card.card_list;
  remaining_tricks : int;
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
    remaining_tricks = 1;
    players = plyrs;
    num_players = p_num;
    round_num = 1;
  }

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

let play_round (rnd : t) players =
  (* Shuffle Deck *)
  match Deck.shuffle rnd.main_deck |> Deck.deal with
  | hands, trump -> ()
  | _ -> failwith "There has been an Error in Deal"

(* Deal Cards to each player *)
(* Distribute cards to each player *)
(* distribute_cars(d, ) *)
(* Betting Round *)
(* How do we do this part? *)

(* Game play round *)
