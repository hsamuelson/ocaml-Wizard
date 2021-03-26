(*main should just print something in the terminal*)
(*to compile and run, input "make play" *)

open ANSITerminal

(* play_game should print the table, and have a function to continually
   poll for inputs I guess, or to just type in inputs. Ask for number of
   players as the input. *)

(* include unicode characters and colors to get suits
0xE2 0x99 0xA4
&#9828;
&#x2664;*)

(** [play_game f] starts the adventure in file [f]. *)
let play_game f : unit =
  print_string [ Bold ] ("you have selected: " ^ f ^ "\n\n");
  print_endline "Please enter the number of players.\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number_string ->
    (**TODO: Catch error if inputting bad information for inputs?*)
      let number = int_of_string number_string in
      if number > 0 && number <= 6 then
        print_string [ Bold ]
          ("you have selected: " ^ string_of_int number ^ "\n\n")
      else
        print_string [ Bold ]
          "number of players must be at least 1 and at most 6"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (*prompt for json file and number of players*)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Wizard Game engine.\n";
  print_endline
    "Please enter the name of the deck json file you want to play with.\n";
  print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
