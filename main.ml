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


  let end_game (finalRound : Round.t) =
    Round.print_scoreboard finalRound;
    (* let score_b = Round.scoreboard (Round.players finalRound) in
    ANSITerminal.print_string
      [ ANSITerminal.cyan; Bold ]
      ("\nRound  \n");
    print_string [] (" Player ID: " ^ fst score_b ^ "\n");
    print_string [] ("\n Score: " ^ snd score_b ^ "\n"); *)
    ANSITerminal.print_string [] "Game over. Thank you for playing wizard! \n\n";
    (** TODO : Print scoreboard and winner*)
    exit 0

let deal_cards num_real_players num_robot_players file =
  let json_file = Yojson.Basic.from_file file in
  let new_table = Table.init_tb num_real_players num_robot_players json_file in

  end_game (Table.run_game new_table)
  
let rec num_ai_players_input_helper num_real_players f : unit= ANSITerminal.print_string
[ ANSITerminal.cyan; Bold ]
"Please enter the number of robot players (at least 0, at most 6).\n\n";
print_string [ Bold ] "> ";
match read_line () with
| exception End_of_file -> ()
| number_string -> (
  (*TODO: Catch error if inputting bad information for inputs?*)
  try
    let number = int_of_string number_string in
    if number >= 0 && number <= 6 then begin
      ANSITerminal.print_string
        [ ANSITerminal.cyan; Bold ]
        ("You have selected: " ^ string_of_int number
       ^ " player(s).\n\n");
       deal_cards num_real_players number f
    end
    else begin
      print_string
        [ Bold; ANSITerminal.red ]
        "Number of robot must be at least 0 and at most 6.\n\n";
        num_ai_players_input_helper num_real_players f
    end
  with Failure e ->
    if e = "Not enough cards" then 
      ()
    else
      print_string
        [ Bold; ANSITerminal.red ]
        "Number of players must be a number that is at least 2 and \
         at most 6.\n\n";
         num_ai_players_input_helper num_real_players f)


let rec num_players_input_helper f : int =
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "Please enter the number of players (at least 2, at most 6).\n\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> 0
  | number_string -> (
      (*TODO: Catch error if inputting bad information for inputs?*)
      try
        let number = int_of_string number_string in
        if number > 1 && number <= 6 then begin
          ANSITerminal.print_string
            [ ANSITerminal.cyan; Bold ]
            ("You have selected: " ^ string_of_int number
           ^ " robot player(s).\n\n");
          number
        end
        else begin
          print_string
            [ Bold; ANSITerminal.red ]
            "Number of players must be at least 2 and at most 6.\n\n";
          num_players_input_helper f
        end
      with Failure e ->
        if e = "Not enough cards" then 
          ()
        else
          print_string
            [ Bold; ANSITerminal.red ]
            "Number of players must be a number that is at least 2 and \
             at most 6.\n\n";
        num_players_input_helper f)

(* [play_game f] starts the adventure in file [f]. *)
let play_game f : unit =
  ANSITerminal.print_string [] ("You have selected: " ^ f ^ "\n\n");
  let num_real = num_players_input_helper f in
  num_ai_players_input_helper num_real f

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

(** I, Peter Munn, found this code from 
https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml, 
which is used to read the lines of a file into a list of strings*)
let read_lines file = 
  let lines = ref [] in
  let channel = open_in file in
  try
    while true; do
      lines := input_line channel :: !lines
    done; !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines ;;


let print_ruleset () = ANSITerminal.print_string
[ ANSITerminal.magenta; ]
((List.fold_left (fun a b -> a ^ "\n" ^ b ) "" (read_lines "rules.txt"))^ "\n\n\n") ; 
print_string [ Bold ] "Press any key to finish reading rules...";
  match read_line () with _ -> 
    ANSITerminal.erase Screen;
ANSITerminal.print_string
[ ANSITerminal.red; Bold] "Those were the rules, now let the game begin! \n \n";() 


let rec main_helper () = ANSITerminal.print_string []
"Press enter to start the game, or type 'rules' to read the rules\n\n";
print_string [ Bold ] "> ";
match read_line () with response -> if response = "rules" then ((print_ruleset ()); deck_input_helper ())
else 
if response = "" then 
deck_input_helper ()
else ANSITerminal.print_string
[ ANSITerminal.red; Bold ]
"\n\n Please either press enter or type 'rules' and press enter\n";
main_helper ()
let main () =
  
  (*prompt for json file and number of players*)
  ANSITerminal.erase Screen;
  PrintFunct.intro_screen ();
  (* PrintFunct.print_hand [{number = 1; suit = "red"};{number = 2, suit = "blue"}] *)
  (* PrintFunct.zero ANSITerminal.red ();
  PrintFunct.one ANSITerminal.green (); 
  PrintFunct.two ANSITerminal.blue (); *)
  ANSITerminal.print_string
    [ ANSITerminal.cyan; Bold ]
    "\n\nWelcome to the 3110 Wizard Game engine.\n";
    main_helper ()
    

(* Execute the game engine. *)
let () = main ()

(* HOW TO START GAME! -from Henry *)
(* Create a new table object *)
(* Call run_game*)
