(* Table will handle betting before round begins *)

type playr_num = int
type t =  {
  (* main_deck : Deck.deck; *)
  players : Player.t list;
  round : Round.t;
  num_players : int;
  scoreboard: int list;
}

let start_round = failwith "Unimplemented"
let start_game = failwith "Unimp"
  (* Initilize game params *)
  (* Call first round *)
(* let () = print_string "Enter number of players" *)
(* let player_num = read_int ()  *)

(* This needs tpo be changed to not a constant once we have multiple
decks *)
(* let deck_size = 52 in *)
(* Initilize players *)
let init_players (p_num : int) =
  let rec init_p_helper p_num p_list = 
    if p_num > 0 then 
      init_p_helper (p_num - 1) (Player.initialize_player :: p_list)
    else p_list
  in
init_p_helper p_num []



(* Number of possible rounds 
/ rounds down so we insure that the last round everyone has the
  correct number of cards*)

let tb (num_p : int) = {
  players = init_players num_p;
  round = Round.init_first_round num_p Deck.deck; (*No way to make a deck*)
  num_players = num_p;
  scoreboard = [];
}  
(* Does this show up correctly in git? *)

let update_scoreboard = failwith "Unimp"
