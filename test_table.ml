open OUnit2
open Table

let p_list = Table.init_players 4

let init_players_helper
    (name : string)
    (expected : int list list)
    (input : int list list) =
  name >:: fun _ -> assert_equal expected input

let init_players_tests =
  [
    p_list
    |> List.map Player.player_to_list
    |> init_players_helper "Init all players"
         [
           [ 0; 0; 0; 0; 0; 0; 1 ];
           [ 0; 0; 0; 0; 0; 0; 2 ];
           [ 0; 0; 0; 0; 0; 0; 3 ];
           [ 0; 0; 0; 0; 0; 0; 4 ];
         ];
  ]

