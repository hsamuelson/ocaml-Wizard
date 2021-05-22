open ANSITerminal
open Sys
open Printexc
open Yojson.Basic.Util

(**[end_game] prints the final scores for every player, a game over
   message, and exits the application*)
let end_game (finalRound : Round.t) =
  Round.print_scoreboard finalRound;
  ANSITerminal.print_string []
    "Game over. Thank you for playing wizard! \n\n";
  exit 0

(**[deal_cards] initializes a game table with [num_real_players]
   players, [num_robot_players] robot players, and [json_file] as the
   deck*)
let deal_cards num_real_players num_robot_players file =
  let json_file = Yojson.Basic.from_file file in
  let new_table =
    Table.init_tb num_real_players num_robot_players json_file
  in
  end_game (Table.run_game new_table)

(**[num_ai_players_input_helper] takes the number of robot players as an
   input, asserts that [num_real_players] + the number of robot players
   is between and including 2 and 6, and if so, deals out the cards from
   deck file [f] to each player *)
let rec num_ai_players_input_helper num_real_players f : unit =
  ANSITerminal.print_string
    [ ANSITerminal.green; Bold ]
    "Please enter the number of robot players (must be at least 2 and \
     at most 6 players total (humans + robots)).\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number_string -> (
      try
        let number = int_of_string number_string in
        let total_number = num_real_players + number in
        if total_number >= 2 && total_number <= 6 then begin
          ANSITerminal.print_string
            [ ANSITerminal.green; Bold ]
            ("You have selected: " ^ string_of_int number
           ^ " robot player(s).\n\n");
          deal_cards num_real_players number f
        end
        else begin
          print_string
            [ Bold; ANSITerminal.green ]
            "Must be at least 2 and at most 6 players total (humans + \
             robots)\n\n";
          num_ai_players_input_helper num_real_players f
        end
      with Failure e ->
        if e = "Not enough cards" then ()
        else
          print_string
            [ Bold; ANSITerminal.red ]
            "must be at least 2 and at most 6 players total (humans + \
             robots)\n\n";
        num_ai_players_input_helper num_real_players f)

(**[num_players_input_helper] take a file [f] and reads keyboard input
   for an integer number of players which must be between and including
   0 and 6 *)
let rec num_players_input_helper f : int =
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "Please enter the number of human players (at least 0, at most 6).\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> 0
  | number_string -> (
      try
        let number = int_of_string number_string in
        if number >= 0 && number <= 6 then begin
          ANSITerminal.print_string
            [ ANSITerminal.cyan; Bold ]
            ("You have selected: " ^ string_of_int number
           ^ " player(s).\n\n");
          number
        end
        else begin
          print_string
            [ Bold; ANSITerminal.red ]
            "Number of players must be at least 0 and at most 6.\n\n";
          num_players_input_helper f
        end
      with Failure e ->
        if e = "Not enough cards" then ()
        else
          print_string
            [ Bold; ANSITerminal.red ]
            "Number of players must be a number that is at least 0 and \
             at most 6.\n\n";
        num_players_input_helper f)

(**[play_game f] starts wizard game with file [f]. *)
let play_game f : unit =
  ANSITerminal.print_string [] ("You have selected: " ^ f ^ "\n\n");
  let num_real = num_players_input_helper f in
  num_ai_players_input_helper num_real f

(**[deck_input_helper] takes in keyboard input and interprets it as a
   json file to use as a wizard deck*)
let rec deck_input_helper () =
  ANSITerminal.print_string []
    "Please enter the name of the deck json file you want to play with \
     (we recommend: main_deck.json).\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
      if Sys.file_exists file_name then play_game file_name
      else
        ANSITerminal.print_string
          [ Bold; ANSITerminal.red ]
          "File not found. \n\n";
      deck_input_helper ()

(**[make_rules] returns a Yojson.Basic.t object from a valid rule json
   file [json]*)
let make_rules json = json |> member "text" |> to_string

(**[print_ruleset] prints the ruleset*)
let print_ruleset () =
  let rules_json = Yojson.Basic.from_file "rules.json" in
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    ("\n\n" ^ make_rules rules_json ^ "\n\n");
  print_string [ Bold ] "Press any key to finish reading rules...";
  match read_line () with
  | _ ->
      ANSITerminal.erase Screen;
      ANSITerminal.print_string
        [ ANSITerminal.red; Bold ]
        "Those were the rules, now let the game begin! \n \n";
      ()

(**[main_helper] will either print the ruleset or continue to starting
   the game depending on the user keyboard input*)
let rec main_helper () =
  ANSITerminal.print_string []
    "Press enter to start the game, or type 'rules' to read the rules\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | response ->
      if response = "rules" then (
        print_ruleset ();
        deck_input_helper ())
      else if response = "" then deck_input_helper ()
      else
        ANSITerminal.print_string
          [ ANSITerminal.red; Bold ]
          "\n\n\
          \ Please either press enter or type 'rules' and press enter\n";
      main_helper ()

(**[check_size_of_screen] keeps checking that you have the correct
   screen size before playing the game*)
let rec check_size_of_screen () =
  let screen_size = size () in
  match screen_size with
  | width, height ->
      if width < 100 then (
        ANSITerminal.print_string [ Bold; red ]
          "Your terminal is not large enough for this game. Please \
           make your terminal wider. Then press ENTER.";
        match read_line () with _ -> check_size_of_screen ())
      else if height < 30 then (
        ANSITerminal.print_string [ Bold; red ]
          "Your terminal is not large enough for this game. Please \
           make your terminal taller. Then press ENTER.";
        match read_line () with _ -> check_size_of_screen ())
      else ()

(**[main] launches the wizard game*)
let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.resize 100 50;
  check_size_of_screen ();
  ANSITerminal.erase Screen;
  PrintFunct.intro_screen ();
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "\n\nWelcome to the 3110 Wizard Game engine.\n";
  main_helper ()

(* Execute the game engine. *)
let () = main ()
