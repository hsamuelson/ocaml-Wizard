open ANSITerminal

let pr_help s = ANSITerminal.print_string [ ANSITerminal.red; Bold ] s

(**[intro_screen] prints the intro screen graphic for the wizard game*)
let intro_screen () =
  let size_screen = size () in
  let left_margin = (fst size_screen / 2) - 35 in
  let right_margin = snd size_screen / 2 in
  set_cursor (left_margin + 1) right_margin;
  pr_help
    "  ▄█     █▄   ▄█   ▄███████▄     \
     ▄████████    ▄████████ \
     ████████▄ ";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "  ███     ███ ███  ██▀     ▄██   \
     ███    ███   ███    ███ ███   \
     ▀███ ";
  move_bol ();

  move_cursor left_margin 1;
  pr_help
    "  ███     ███ ███▌       ▄███▀   \
     ███    ███   ███    ███ ███    \
     ███";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "  ███     ███ ███▌  \
     ▀█▀▄███▀▄▄   ███    ███  \
     ▄███▄▄▄▄██▀ ███    ███ ";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "  ███     ███ ███▌   ▄███▀   ▀ \
     ▀███████████ \
     ▀▀███▀▀▀▀▀   ███    ███ ";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "  ███     ███ ███  ▄███▀         \
     ███    ███ ▀███████████ \
     ███    ███ ";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "  ███ ▄█▄ ███ ███  ███▄     \
     ▄█   ███    ███   ███    ███ \
     ███   ▄███ ";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "   ▀███▀███▀  █▀    \
     ▀████████▀   ███    █▀    \
     ███    ███ ████████▀ ";
  move_bol ();
  move_cursor left_margin 1;
  pr_help
    "                                              ███    \
     ███          ";
  move_cursor 0 1;
  move_bol ();

  ANSITerminal.print_string []
    "                                                 CS3110 Spring 2021\n";
  ANSITerminal.print_string []
    "                                              pcm82, ml2359, \
     hes227";
  move_cursor 0 10

(**[get_index] returns the string graphical representation of the index
   [idx]*)
let get_index idx =
  if idx < 10 then "█ Index: " ^ string_of_int idx ^ "        █"
  else "█ Index: " ^ string_of_int idx ^ "       █"

(**[pr] prints string s with color [clr]*)
let pr s clr =
  save_cursor ();
  ANSITerminal.print_string [ ANSITerminal.Bold; clr ] s;
  restore_cursor ();
  move_cursor 0 1

(**[get_row_col idx] returns the placement of the card in the terminal*)
let get_new_row idx =
  (*card width: 19 + 6 spaces*)
  let width_terminal = fst (size ()) in
  let max_idx_per_row = width_terminal / 25 in
  if idx >= max_idx_per_row then true else false

(** [helper_move_cursor idx] helps to move the cursor before each card
    is printed*)
let helper_move_cursor idx : unit =
  let new_row = get_new_row idx in
  if new_row then (
    ANSITerminal.erase Screen;
    move_bol ())
  else ()

(**[zero] prints the graphical representation of 0 at index [idx] with
   color [clr]*)
let zero clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄█████▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ▀█████▀     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[one] prints the graphical representation of 1 at index [idx] with
   color [clr]*)
let one clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█      ▄███       █" clr;
  pr "█        ██       █" clr;
  pr "█        ██       █" clr;
  pr "█        ██       █" clr;
  pr "█      ▄████▄     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[two] prints the graphical representation of 2 at index [idx] with
   color [clr]*)
let two clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄█████▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█        ▄██▀     █" clr;
  pr "█      ▄██▀       █" clr;
  pr "█    ▄████▄▄▄     █" clr;
  pr "█    ▀▀▀▀▀▀▀▀     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[three] prints the graphical representation of 3 at index [idx] with
   color [clr]*)
let three clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄█████▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█        ▄██▀     █" clr;
  pr "█        ▀██▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ▀█████▀     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[four] prints the graphical representation of 4 at index [idx] with
   color [clr]*)
let four clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄█   █▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ▀██████     █" clr;
  pr "█          ██     █" clr;
  pr "█          ██     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[five] prints the graphical representation of 5 at index [idx] with
   color [clr]*)
let five clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█    ███████▀     █" clr;
  pr "█    ██           █" clr;
  pr "█    ▀██████▄     █" clr;
  pr "█         ▀██     █" clr;
  pr "█    ▄▄    ██     █" clr;
  pr "█    ▀██████▀     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[six] prints the graphical representation of 6 at index [idx] with
   color [clr]*)
let six clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█    ▄█████▀      █" clr;
  pr "█    ██           █" clr;
  pr "█    ██           █" clr;
  pr "█    ███████▄     █" clr;
  pr "█    ██   ▀██     █" clr;
  pr "█    ██    ██     █" clr;
  pr "█    ▀██████▀     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[seven] prints the graphical representation of 7 at index [idx] with
   color [clr]*)
let seven clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄███████▄   █" clr;
  pr "█           ▄█▀   █" clr;
  pr "█          ▄█▀    █" clr;
  pr "█         ▄█      █" clr;
  pr "█        ▄█▀      █" clr;
  pr "█       ▄█▀       █ " clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[eight] prints the graphical representation of 8 at index [idx] with
   color [clr]*)
let eight clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄█████▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ██▄▄██▀     █" clr;
  pr "█     ▄██▀██▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ▀█████▀     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[nine]] prints the graphical representation of 9 at index [idx] with
   color [clr]*)
let nine clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█     ▄█████▄     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ██   ██     █" clr;
  pr "█     ▀██████     █" clr;
  pr "█          ██     █" clr;
  pr "█          ██     █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[ten] prints the graphical representation of 10 at index [idx] with
   color [clr]*)
let ten clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█  ▄███  ▄█████▄  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█  ▄███▄ ▀█████▀  █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[eleven] prints the graphical representation of 11 at index [idx]
   with color [clr]*)
let eleven clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█   ▄███   ▄███   █" clr;
  pr "█     ██     ██   █" clr;
  pr "█     ██     ██   █" clr;
  pr "█     ██     ██   █" clr;
  pr "█     ██     ██   █" clr;
  pr "█   ▄███▄  ▄███▄  █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[twelve] prints the graphical representation of 12 at index [idx]
   with color [clr]*)
let tweleve clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█  ▄███  ▄█████▄  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█    ██     ▄█▀   █" clr;
  pr "█    ██   ▄██▀    █" clr;
  pr "█    ██  ██   ▄▄  █" clr;
  pr "█  ▄███▄ ██████▀  █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[thirteen] prints the graphical representation of 13 at index [idx]
   with color [clr]*)
let thirteen clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█  ▄███  ▄█████▄  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█    ██    ▄██▀   █" clr;
  pr "█    ██    ▀██▄   █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█  ▄███▄ ▀█████▀  █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
  restore_cursor ()

(**[fourteen] prints the graphical representation of 14 at index [idx]
   with color [clr]*)
let fourteen clr idx () =
  pr "▄█████████████████▄" clr;
  pr (get_index idx) clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█  ▄███  ▄█   █▄  █" clr;
  pr "█    ██  ██   ██  █" clr;
  pr "█    ██  ▀██████  █" clr;
  pr "█    ██       ██  █" clr;
  pr "█    ██       ██  █" clr;
  pr "█  ▄███▄      █▀  █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "█                 █" clr;
  pr "▀█████████████████▀" clr;
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
        if not (get_new_row i) then
          select_number (Card.get_num h)
            (get_card_color (Card.get_suit h))
            i
        else restore_cursor ();
        aux t (i + 1)
    | [] -> ()
  in
  save_cursor ();
  set_cursor 0 1;
  aux c_list i;
  restore_cursor ()
