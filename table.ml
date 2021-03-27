(* Table will handle betting before round begins *)

type playr_num = int

type t = {
  (* main_deck : Deck.deck; *)
  (* players : Player.t list; *)
  round : Round.t;
  num_players : int;
  scoreboard : int list;
  round_num : int;
}

type scoreboard = {
  player_ids : Player.t list;
  scores : int list;
}

let run_game (tb : t) =
  (* At the moment we only play a single round *)
  Round.play_round tb.round

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
    scoreboard = [];
    round_num = 1;
  }

let scoreboard (p_list : Player.t list) =
  let rec sb_helper p_list (pair : int * int) list =
    match p_list with hd :: tl -> [] | _ -> []
  in
  sb_helper p_list
