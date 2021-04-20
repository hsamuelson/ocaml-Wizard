(* main should just print something in the terminal*)
(*to compile and run, input "make play" *)

(* >> brew install cloc >> cloc --by-file --include-lang=OCaml . *)

open ANSITerminal
open Sys
open Printexc

(* play_game should print the table, and have a function to continually
   poll for inputs I guess, or to just type in inputs. Ask for number of
   players as the input. *)

(* include unicode characters and colors to get suits 0xE2 0x99 0xA4
   &#9828; &#x2664;*)

(** [deal cards] is an example of how we can display information on the
    terminal of the players. Will not be used in the final game!*)
let deal_cards num_players file =
  let json_file = Yojson.Basic.from_file file in
  let deck = Deck.make_deck json_file in
  let dealed_cards = Deck.deal (Deck.shuffle deck) num_players 5 in
  let decks =
    match dealed_cards with deal, trump -> Array.of_list deal
  in

  for i = 0 to num_players - 1 do
    let player =
      Player.initialize_player i
      |> Player.give_cards decks.(i)
      |> Player.make_bet 2 |> Player.win_trick |> Player.win_trick
      |> Player.choose_card "next"
      |> Player.choose_card "next"
    in
    Player.print_player player
  done

let deal_cards_2 num_players file =
  let json_file = Yojson.Basic.from_file file in
  let new_table = Table.init_tb num_players json_file in

  Table.run_game new_table;
  ()

let end_game () =
  ANSITerminal.print_string [] "Game over. Thank you for playing wizard! \n\n";
  (** TODO : Print scoreboard and winner*)
  exit 0

let rec num_players_input_helper f =
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "Please enter the number of players (at least 2, at most 6).\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number_string -> (
      (*TODO: Catch error if inputting bad information for inputs?*)
      try
        let number = int_of_string number_string in
        if number > 1 && number <= 6 then begin
          ANSITerminal.print_string
            [ ANSITerminal.cyan; Bold ]
            ("You have selected: " ^ string_of_int number
           ^ " player(s).\n\n");
          deal_cards_2 number f
        end
        else
          print_string
            [ Bold; ANSITerminal.red ]
            "Number of players must be at least 2 and at most 6.\n\n";
        num_players_input_helper f
      with Failure e ->
        if e = "Not enough cards" then end_game ()
        else
          print_string
            [ Bold; ANSITerminal.red ]
            "Number of players must be a number that is at least 2 and \
             at most 6.\n\n";
        num_players_input_helper f)

(* [play_game f] starts the adventure in file [f]. *)
let play_game f : unit =
  ANSITerminal.print_string [] ("You have selected: " ^ f ^ "\n\n");
  num_players_input_helper f

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

(* [main ()] prompts for the game to play, then starts it. *)
let main () =
  (*prompt for json file and number of players*)
  ANSITerminal.erase Screen;
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "\n\nWelcome to the 3110 Wizard Game engine.\n";
  deck_input_helper ()

(* Execute the game engine. *)
let () = main ()

(* HOW TO START GAME! -from Henry *)
(* Create a new table object *)
(* Call run_game*)
