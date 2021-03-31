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
  if tb.round_num < tb.num_players / 52 then
    run_game
      {
        round = Round.play_round tb.round;
        num_players = tb.num_players;
        round_num = tb.round_num + 1;
      }
  else tb.round

(* Initialize players *)
let init_players (p_num : int) : Player.t list =
  let rec init_p_helper p_num p_list =
    if p_num > 0 then
      init_p_helper (p_num - 1)
        (Player.initialize_player p_num :: p_list)
    else p_list
  in
  init_p_helper p_num []

(* Initilize a table object *)
let init_tb (num_p : int) json_file =
  {
    round =
      Round.init_first_round num_p
        (Deck.make_deck json_file)
        (init_players num_p);
    num_players = num_p;
    round_num = 1;
  }

let rec list_to_string acc lst =
  match lst with
  | [] -> acc
  | hd :: tl -> list_to_string (acc ^ " " ^ string_of_int hd) tl

let scoreboard (p_list : Player.t list) =
  let scores =
    p_list
    |> List.map (fun x -> Player.player_score x)
    |> List.map string_of_int
  in
  let ids =
    p_list
    |> List.map (fun x -> Player.player_id x)
    |> List.map string_of_int
  in
  (ids, scores)
