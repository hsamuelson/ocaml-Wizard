
type t = {
  main_deck : Deck.deck;
  remaining_tricks : int;
  (* tricks : trick list; *)
  (* trump_suit : Card.suit; *)
  (* leader : Player.t;  *) (*<- leader will be rotated in round at
  the end of each round.*)
  num_players : int;
  (* Round_num is the number of the round, each time a new 
      round is created we will increment this number*)
  round_num : int;
}

let init_first_round p_num dck= {
  main_deck = dck;
  remaining_tricks = 1; (*First round we start with one trick*)
  num_players = p_num;
  round_num = 1;
}
let play_round (rnd : t) players = 
  (* Shuffle Deck *)
  Deck.shuffle rnd.main_deck
  (* Deal Cards to each player *)
  (* Distribute cards to each player *)
  (* distribute_cars(d, ) *)
  (* Betting Round *)
  (* How do we do this part? *)

  (* Game play round *)

  

  (* Creates a new list of players all with cards *)
(* let distribute_cars s_deck num_players players = 
  let hands = (Deck.deal num_players rnd.main_deck) in
  let output_players = match players with
  | hd::tl -> begin
    match s_deck with
    | head::tail -> failwith "Un"
    | _ -> failwith "Error"
  end
  | _ -> failwith "Error" in *)