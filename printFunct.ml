(**Represents a printer object to print cards graphically*)
open ANSITerminal

(**[intro_screen] prints the intro screen graphic for the wizard game*)
let intro_screen () =
  ANSITerminal.print_string
    [ ANSITerminal.red; Bold ]
    " \n\
    \  ▄█     █▄   ▄█   ▄███████▄     \
     ▄████████    ▄████████ \
     ████████▄  \n\
    \  ███     ███ ███  ██▀     ▄██   \
     ███    ███   ███    ███ ███   \
     ▀███ \n\
    \  ███     ███ ███▌       ▄███▀   \
     ███    ███   ███    ███ ███    \
     ███ \n\
    \  ███     ███ ███▌  \
     ▀█▀▄███▀▄▄   ███    ███  \
     ▄███▄▄▄▄██▀ ███    ███ \n\
    \  ███     ███ ███▌   ▄███▀   ▀ \
     ▀███████████ \
     ▀▀███▀▀▀▀▀   ███    ███ \n\
    \  ███     ███ ███  ▄███▀         \
     ███    ███ ▀███████████ \
     ███    ███ \n\
    \  ███ ▄█▄ ███ ███  ███▄     \
     ▄█   ███    ███   ███    ███ \
     ███   ▄███ \n\
    \   ▀███▀███▀  █▀    \
     ▀████████▀   ███    █▀    \
     ███    ███ ████████▀  \n\
    \                                              ███    \
     ███            \n";

  ANSITerminal.print_string []
    "                                                 CS3110 Spring 2021\n";
  ANSITerminal.print_string []
    "                                              pcm82, ml2359, \
     hes227"

(***[get_index] returns the string graphical representation of the index
  [idx]*)
let get_index idx =
  if idx < 10 then "█ Index: " ^ string_of_int idx ^ "        █"
  else "█ Index: " ^ string_of_int idx ^ "       █"

(***[zero] prints the graphical representation of 0*)
let zero clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄█████▄     █";
  pr "█     ██   ██     █";
  pr "█     ██   ██     █";
  pr "█     ██   ██     █";
  pr "█     ██   ██     █";
  pr "█     ▀█████▀     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";
  restore_cursor ()

(***[one] prints the graphical representation of 1*)
let one clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in

  save_cursor ();

  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█      ▄███       █";
  pr "█        ██       █";
  pr "█        ██       █";
  pr "█        ██       █";
  pr "█      ▄████▄     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[two] prints the graphical representation of 2*)
let two clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄█████▄     █";
  pr "█     ██   ██     █";
  pr "█        ▄██▀     █";
  pr "█      ▄██▀       █";
  pr "█    ▄████▄▄▄     █";
  pr "█    ▀▀▀▀▀▀▀▀     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[three] prints the graphical representation of 3*)
let three clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄█████▄     █";
  pr "█     ██   ██     █";
  pr "█        ▄██▀     █";
  pr "█        ▀██▄     █";
  pr "█     ██   ██     █";
  pr "█     ▀█████▀     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[four] prints the graphical representation of 4*)
let four clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄█   █▄     █";
  pr "█     ██   ██     █";
  pr "█     ██   ██     █";
  pr "█     ▀██████     █";
  pr "█          ██     █";
  pr "█          ██     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";
  restore_cursor ()

(***[five] prints the graphical representation of 5*)
let five clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█    ███████▀     █";
  pr "█    ██           █";
  pr "█    ▀██████▄     █";
  pr "█         ▀██     █";
  pr "█    ▄▄    ██     █";
  pr "█    ▀██████▀     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[six] prints the graphical representation of 6*)
let six clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█    ▄█████▀      █";
  pr "█    ██           █";
  pr "█    ██           █";
  pr "█    ███████▄     █";
  pr "█    ██   ▀██     █";
  pr "█    ██    ██     █";
  pr "█    ▀██████▀     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[seven] prints the graphical representation of 7*)
let seven clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄███████▄   █";
  pr "█           ▄█▀   █";
  pr "█          ▄█▀    █";
  pr "█         ▄█      █";
  pr "█        ▄█▀      █";
  pr "█       ▄█▀       █ ";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[eight] prints the graphical representation of 8*)
let eight clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄█████▄     █";
  pr "█     ██   ██     █";
  pr "█     ██▄▄██▀     █";
  pr "█     ▄██▀██▄     █";
  pr "█     ██   ██     █";
  pr "█     ▀█████▀     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[nine]] prints the graphical representation of 9*)
let nine clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█     ▄█████▄     █";
  pr "█     ██   ██     █";
  pr "█     ██   ██     █";
  pr "█     ▀██████     █";
  pr "█          ██     █";
  pr "█          ██     █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[ten] prints the graphical representation of 10*)
let ten clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█  ▄███  ▄█████▄  █";
  pr "█    ██  ██   ██  █";
  pr "█    ██  ██   ██  █";
  pr "█    ██  ██   ██  █";
  pr "█    ██  ██   ██  █";
  pr "█  ▄███▄ ▀█████▀  █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[eleven] prints the graphical representation of 11*)
let eleven clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█   ▄███   ▄███   █";
  pr "█     ██     ██   █";
  pr "█     ██     ██   █";
  pr "█     ██     ██   █";
  pr "█     ██     ██   █";
  pr "█   ▄███▄  ▄███▄  █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀"

;;
restore_cursor ()

(***[twelve] prints the graphical representation of 12*)
let tweleve clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█  ▄███  ▄█████▄  █";
  pr "█    ██  ██   ██  █";
  pr "█    ██     ▄█▀   █";
  pr "█    ██   ▄██▀    █";
  pr "█    ██  ██   ▄▄  █";
  pr "█  ▄███▄ ██████▀  █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[thirteen] prints the graphical representation of 13*)
let thirteen clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█  ▄███  ▄█████▄  █";
  pr "█    ██  ██   ██  █";
  pr "█    ██    ▄██▀   █";
  pr "█    ██    ▀██▄   █";
  pr "█    ██  ██   ██  █";
  pr "█  ▄███▄ ▀█████▀  █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(***[fourteen] prints the graphical representation of 14*)
let fourteen clr idx () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr (get_index idx);
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█  ▄███  ▄█   █▄  █";
  pr "█    ██  ██   ██  █";
  pr "█    ██  ▀██████  █";
  pr "█    ██       ██  █";
  pr "█    ██       ██  █";
  pr "█  ▄███▄      █▀  █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "█                 █";
  pr "▀█████████████████▀";

  restore_cursor ()

(**[select_number] selects which number to print based on [number] with
   position deteremined by [idx]*)
let select_number number clr idx =
  match number with
  | 0 -> zero clr idx ()
  | 1 -> one clr idx ()
  | 2 -> two clr idx ()
  | 3 -> three clr idx ()
  | 4 -> four clr idx ()
  | 5 -> five clr idx ()
  | 6 -> six clr idx ()
  | 7 -> seven clr idx ()
  | 8 -> eight clr idx ()
  | 9 -> nine clr idx ()
  | 10 -> ten clr idx ()
  | 11 -> eleven clr idx ()
  | 12 -> tweleve clr idx ()
  | 13 -> thirteen clr idx ()
  | 14 -> fourteen clr idx ()
  | _ -> failwith "Error"

(**[get_card_color] returns the ANSIterminal color associated with
   card_suit [card_suit]*)
let get_card_color card_suit =
  match card_suit with
  | "red" -> ANSITerminal.red
  | "blue" -> ANSITerminal.cyan
  | "green" -> ANSITerminal.green
  | "yellow" -> ANSITerminal.yellow
  | _ -> ANSITerminal.white

(**[print_hand] prints the graphic representing card list [c_list]*)
let print_hand (c_list : Card.card list) i =
  let rec aux c_list i =
    match c_list with
    | h :: t ->
        set_cursor (i * 25) 1;
        select_number (Card.get_num h)
          (get_card_color (Card.get_suit h))
          i;
        aux t (i + 1)
    | [] -> ()
  in
  save_cursor ();
  set_cursor 0 1;
  aux c_list i;
  restore_cursor ()
