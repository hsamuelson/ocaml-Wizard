(* Table will handle betting before round begins *)

type playr_num = int

type t = {
  (* main_deck : Deck.deck; *)
  (* players : Player.t list; *)
  round : Round.t;
  num_players : int;
  round_num : int;
}

let rec run_game (tb : t) =
  if tb.round_num <= Round.deck_size tb.round / tb.num_players then
    run_game
      {
        round = Round.play_round tb.round;
        num_players = tb.num_players;
        round_num = tb.round_num + 1;
      }
  else tb.round

let rec init_normal_helper p_num p_list =
  if p_num > 0 then
    init_normal_helper (p_num - 1)
      (Player.initialize_player p_num false :: p_list)
  else p_list

let rec init_robot_helper num_normal_players robot_players p_list =
  if robot_players > 0 then
    init_robot_helper num_normal_players (robot_players - 1)
      (Player.initialize_player
         (num_normal_players + robot_players)
         true
      :: p_list)
  else p_list

(* Initialize players *)
let init_players (p_num : int) (robot_players : int) : Player.t list =
  let normal_list = init_normal_helper p_num [] in
  let robot_list = init_robot_helper p_num robot_players [] in
  normal_list @ robot_list

(* Initilize a table object *)
let init_tb (num_p : int) (num_robot_players : int) json_file =
  {
    round =
      Round.init_first_round
        (num_p + num_robot_players)
        (Deck.make_deck json_file)
        (init_players num_p num_robot_players);
    num_players = num_p + num_robot_players;
    round_num = 1;
  }
