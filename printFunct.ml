open ANSITerminal

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

let get_index idx =
  if idx < 10 then "█ Index: " ^ string_of_int idx ^ "        █"
  else "█ Index: " ^ string_of_int idx ^ "       █"

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

let get_card_color card_suit =
  match card_suit with
  | "red" -> ANSITerminal.red
  | "blue" -> ANSITerminal.cyan
  | "green" -> ANSITerminal.green
  | "yellow" -> ANSITerminal.yellow
  | _ -> ANSITerminal.white

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
