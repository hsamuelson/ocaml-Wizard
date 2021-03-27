(*main should just print something in the terminal*)
(*to compile and run, input "make play" *)

(* >> brew install cloc >> cloc --by-file --include-lang=OCaml . *)

open ANSITerminal

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
      fst
        (Player.initialize_player i
        |> Player.give_cards decks.(i)
        |> Player.make_bet 2 |> Player.win_trick |> Player.win_trick
        |> Player.choose_card "next"
        |> Player.choose_card "next"
        |> Player.play_card)
      |> Player.finish_round
    in
    print_endline (Player.player_to_string player)
  done

(* [play_game f] starts the adventure in file [f]. *)
let play_game f : unit =
  print_string [ Bold ] ("you have selected: " ^ f ^ "\n\n");
  print_endline "Please enter the number of players.\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number_string ->
      (*TODO: Catch error if inputting bad information for inputs?*)
      let number = int_of_string number_string in
      if number > 0 && number <= 6 then begin
        print_string [ Bold ]
          ("you have selected: " ^ string_of_int number
         ^ " player(s).\n\n");
        deal_cards number f
      end
      else
        print_string [ Bold ]
          "number of players must be at least 1 and at most 6"

(* [main ()] prompts for the game to play, then starts it. *)
let main () =
  (*prompt for json file and number of players*)
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "\n\nWelcome to the 3110 Wizard Game engine.\n";
  print_endline
    "Please enter the name of the deck json file you want to play with.\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()

(* HOW TO START GAME! -from Henry *)
(* Create a new table object *)
(*  Call run_game*)