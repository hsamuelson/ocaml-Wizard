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

let zero clr () =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; clr ]
    "\n\
    \  ▄█████████████████▄\n\
    \  █                 █\n\
    \  █                 █\n\
    \  █                 █\n\
    \  █                 █\n\
    \  █     ▄████▄      █\n\
    \  █     █    █      █\n\
    \  █     █    █      █\n\
    \  █     █    █      █\n\
    \  █     ▀████▀      █\n\
    \  █                 █\n\
    \  █                 █\n\
    \  █                 █\n\
    \  █                 █\n\
    \  █                 █\n\
    \  ▀█████████████████▀\n\
    \  \n\
    \  \n\
    \  "

let one clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in

  save_cursor ();

  pr "▄█████████████████▄";
  pr "█                 █";
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

let two clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let three clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let four clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let five clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let six clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let seven clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let eight clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let nine clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let ten clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let eleven clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let tweleve clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let thirteen clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let fourteen clr () =
  let pr s =
    save_cursor ();
    ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
    restore_cursor ();
    move_cursor 0 1
  in
  pr "▄█████████████████▄";
  pr "█                 █";
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

let select_number number clr =
  match number with
  | 0 -> zero clr ()
  | 1 -> one clr ()
  | 2 -> two clr ()
  | 3 -> three clr ()
  | 4 -> four clr ()
  | 5 -> five clr ()
  | 6 -> six clr ()
  | 7 -> seven clr ()
  | 8 -> eight clr ()
  | 9 -> nine clr ()
  | 10 -> ten clr ()
  | 11 -> eleven clr ()
  | 12 -> tweleve clr ()
  | 13 -> thirteen clr ()
  | 14 -> fourteen clr ()
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
        (* let w,h = size () in (* set_cursor (1) (1); *) (* move_cursor
           (20+(i*10)) 0; *) set_cursor ((w/3)*i) (-1); one
           ANSITerminal.green (); (* print_string [] "hello\n\n"; *) (*
           save_cursor (); *) aux t (i+1) *)
        set_cursor (i * 25) 1;
        select_number (Card.get_num h)
          (get_card_color (Card.get_suit h));
        aux t (i + 1)
    | [] -> ()
  in
  save_cursor ();
  set_cursor 0 1;
  aux c_list i;
  restore_cursor ()
