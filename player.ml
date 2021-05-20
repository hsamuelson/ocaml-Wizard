open Unix
open Float
open ANSITerminal

exception OutOfBounds

exception NoCardsLeft

exception NotValidSelection

type t = {
  bet : int;  (** This player's current bet for this trick. *)
  tricks_won_this_round : int;
      (** The number of tricks this player has already won. *)
  current_score : int;
      (** The current score accross muiltple rounds. *)
  current_hand : Card.card list;
      (** The number of rounds this player has already won. *)
  current_selected_card : Card.card;
      (** The card we are focused on (current selection)*)
  current_selected_index : int;  (** Index of currently selected card*)
  player_id : int;  (** the id of the player *)
  is_robot : bool;  (** whether the player is a robot*)
}

let next = "next"

let prev = "prev"

(** [initialize_player] initializes all fields of a player object
    [p_num], and makes the player a robot if [robot] *)
let initialize_player (p_num : int) (robot : bool) =
  {
    bet = 0;
    tricks_won_this_round = 0;
    current_score = 0;
    current_hand = [];
    current_selected_card = Card.make_no_card ();
    current_selected_index = 0;
    player_id = p_num;
    is_robot = robot;
  }

(** [reset_round_player] resets all necessary parts of a player object
    [player]. *)
let reset_round_player (player : t) =
  {
    player with
    bet = 0;
    tricks_won_this_round = 0;
    current_hand = [];
    current_selected_index = 0;
    current_selected_card = Card.make_no_card ();
  }

(** [get_hand_size] returns the number of cards that are in [hand] *)
let rec get_hand_size hand =
  match hand with [] -> 0 | h :: t -> 1 + get_hand_size t

(** [find_card_at_index] returns the card object in [card_list] at
    index[index]. Raises OutOfBounds if [index] is outside the list's
    capacity*)
let rec find_card_at_index (card_list : Card.card list) index =
  match index with
  | 0 -> ( match card_list with [] -> raise NoCardsLeft | h :: t -> h)
  | _ -> (
      match card_list with
      | [] -> raise OutOfBounds
      | h :: t -> if t = [] then h else find_card_at_index t (index - 1)
      )

(** [select_previous_card] returns the card object in the player
    [player]'s hand at the previous selected index*)
let select_previous_card (player : t) =
  match player with
  | { current_hand = hand; current_selected_index = index; _ } ->
      let new_idx =
        if index = 0 then get_hand_size hand - 1 else index - 1
      in
      {
        player with
        current_selected_index = new_idx;
        current_selected_card = find_card_at_index hand new_idx;
      }

(** [select_next_card] returns the card object in the player [player]'s
    hand at the next selected index*)
let select_next_card (player : t) =
  match player with
  | { current_hand = hand; current_selected_index = index; _ } ->
      let new_idx =
        if index = get_hand_size hand - 1 then 0 else index + 1
      in
      {
        player with
        current_selected_index = new_idx;
        current_selected_card = find_card_at_index hand new_idx;
      }

(** [choose_card_at_index] chooses the card at index [index] in player
    [player]'s hand*)
let choose_card_at_index (player : t) (index : int) =
  let new_index =
    if index >= get_hand_size player.current_hand then
      get_hand_size player.current_hand - 1
    else if index < 0 then 0
    else index
  in
  let new_card = find_card_at_index player.current_hand new_index in
  ( {
      player with
      current_selected_index = new_index;
      current_selected_card = new_card;
    },
    new_card )

(** [choose_card] either chooses the card at the current selection that
    player [player] is holding or it selects the next or prev card based
    on [move]*)
let choose_card move (player : t) =
  if move = next || move = prev then
    if move = prev then select_previous_card player
    else select_next_card player
  else raise NotValidSelection

(** [remove_index a b] removes the card from the player's hand at the
    given index. If the index is out of bounds, return OutOfBounds.
    Returns the new list of cards without the index card.*)
let rec remove_index player_list index =
  match index with
  | 0 -> (
      match player_list with [] -> raise OutOfBounds | h :: t -> t)
  | _ -> (
      match player_list with
      | [] -> raise OutOfBounds
      | h :: t -> h :: remove_index t (index - 1))

(** [remove_current_selected_card] returns a new player without the card
    in hand that is currently selected for player [player]. Used in
    play_card. If the selected card is 0, we keep the selected index at
    0. Otherwise, we make the new selected card at previous_index -1*)
let remove_current_selected_card (player : t) =
  let curr_hand = player.current_hand in
  let curr_index = player.current_selected_index in
  {
    player with
    current_hand = remove_index curr_hand curr_index;
    current_selected_index =
      (if curr_index = 0 then curr_index else curr_index - 1);
    current_selected_card =
      (if curr_index = 0 then
       let size = get_hand_size curr_hand in
       if size < 2 then Card.make_no_card ()
       else find_card_at_index curr_hand (curr_index + 1)
      else find_card_at_index curr_hand (curr_index - 1));
  }

(** [win_trick] allows player [player] to win a trick and returns the
    current state of the player *)
let win_trick (player : t) =
  {
    player with
    tricks_won_this_round = player.tricks_won_this_round + 1;
  }

(** [finish_round ] Finishes the round and adds trick points to the
    current player [player]'s score *)
let finish_round (player : t) =
  match player with
  | {
   current_score = curr_score;
   bet;
   tricks_won_this_round = tricks;
   _;
  } ->
      if tricks = bet then
        { player with current_score = curr_score + bet + 1 }
      else
        {
          player with
          current_score = curr_score - Int.abs (bet - tricks);
        }

(** [is_robot] returns whether the given player [player] is a robot*)
let get_is_robot player = player.is_robot

(** [give_cards a b] returns a new player as a copy of b with new hand a*)
let give_cards lst player =
  {
    player with
    current_hand = lst;
    current_selected_card =
      (match lst with [] -> Card.make_no_card () | h :: t -> h);
  }

(** [card_list_to_string a b] Makes the list of cards into a string*)
let rec card_list_to_string lst acc =
  match lst with
  | [] -> acc
  | h :: t -> card_list_to_string t (acc ^ Card.string_of_card h)

(** [get_player_hand t] returns the stirng of the players hand *)
let get_player_hand player = card_list_to_string player.current_hand ""

(** [player_to_list p] returns an int list representation of player*)
let player_to_list (player : t) =
  match player with
  | {
   bet = b;
   tricks_won_this_round = t;
   current_score = c;
   current_hand = ch;
   current_selected_card = cc;
   current_selected_index = csi;
   player_id = pi;
   _;
  } ->
      [ b; t; c; get_hand_size ch; Card.get_num cc; csi; pi ]

(** [hand_to_string a b] returns the string representation of a list of
    cards **)
let rec hand_to_string hand acc =
  match hand with
  | [] -> acc
  | h :: t -> hand_to_string t (acc ^ Card.string_of_card h)

(** [player_to_string] returns a string representation of a player*)
let player_to_string (player : t) =
  match player with
  | {
   bet = b;
   tricks_won_this_round = t;
   current_score = c;
   current_hand = ch;
   current_selected_card = cc;
   current_selected_index = csi;
   player_id = pi;
   _;
  } ->
      "Player " ^ string_of_int pi ^ " information: \nCurrent bet: "
      ^ string_of_int b ^ "\nTricks won this round: " ^ string_of_int t
      ^ "\nCurrent score: " ^ string_of_int c ^ "\nCurrent hand: "
      ^ hand_to_string ch "" ^ "\nCurrently selected card: "
      ^ Card.string_of_card cc ^ "\nCurrently selected index: "
      ^ string_of_int csi ^ "\n"

(** [make_bet] returns player [player] with bet [bet]*)
let make_bet bet (player : t) = { player with bet }

(** [make_bet] returns player [plyr]'s score*)
let player_score (plyr : t) = plyr.current_score

(** [make_bet] returns player [plyr]'s id*)
let player_id (plyr : t) = plyr.player_id

(** [get_card_color] returns the associated color of a card suit
    [card_suit]*)
let get_card_color card_suit =
  match card_suit with
  | "red" -> ANSITerminal.red
  | "blue" -> ANSITerminal.cyan
  | "green" -> ANSITerminal.green
  | "yellow" -> ANSITerminal.yellow
  | _ -> ANSITerminal.white

(** [print_cards_with_colors] prints the cards in [card_list] with
    colors in a big format*)
let rec print_cards_with_colors card_list =
  match card_list with
  | h :: t ->
      ANSITerminal.print_string
        [ get_card_color (Card.get_suit h); Bold ]
        (Card.string_of_card h);
      print_cards_with_colors t
  | [] -> ()

(** [print_cards_with_colors_short] prints the cards in [card_list] with
    colors in a small format*)
let rec print_cards_with_colors_short card_list =
  match card_list with
  | h :: t ->
      ANSITerminal.print_string
        [ get_card_color (Card.get_suit h); Bold ]
        ("|" ^ string_of_int (Card.get_num h) ^ "| ");
      print_cards_with_colors_short t
  | [] -> ()

(**[print_player] prints the current information for player [player]*)
let print_player (player : t) =
  match player with
  | {
   bet = b;
   tricks_won_this_round = t;
   current_score = c;
   current_hand = ch;
   current_selected_card = cc;
   current_selected_index = csi;
   player_id = pi;
   _;
  } ->
      PrintFunct.print_hand ch 0;
      move_bol ();
      let curr_pos = pos_cursor () in
      set_cursor 0 (snd curr_pos + 2);
      save_cursor ();
      ANSITerminal.print_string
        [ ANSITerminal.magenta; Underlined; Bold ]
        ("●○●○●○●○● Player " ^ string_of_int pi
       ^ " ●○●○●○●○●");
      restore_cursor ();
      move_cursor 0 1;
      save_cursor ();
      ANSITerminal.print_string [] ("Current bet: " ^ string_of_int b);
      restore_cursor ();
      move_cursor 0 1;
      save_cursor ();
      ANSITerminal.print_string []
        ("Tricks won this round: " ^ string_of_int t);
      restore_cursor ();
      move_cursor 0 1;
      save_cursor ();
      ANSITerminal.print_string [] ("Current score: " ^ string_of_int c);
      restore_cursor ();
      move_cursor 0 1;
      save_cursor ();
      ANSITerminal.print_string [] "Current hand: ";
      print_cards_with_colors_short ch;
      restore_cursor ();
      move_cursor 0 1;
      save_cursor ();
      ANSITerminal.print_string [] "Currently selected card: ";
      print_cards_with_colors_short [ cc ];
      restore_cursor ();
      move_cursor 0 1;
      save_cursor ();
      ANSITerminal.print_string
        [ ANSITerminal.magenta; Underlined; Bold ]
        "●○●○●○●○●○●○●○●○●○●○●○●○●○●○";
      restore_cursor ()

(**[print_player_list] prints the player information for each player in
   [list_players]*)
let print_player_list list_players =
  print_endline
    ("players list length: "
    ^ string_of_int (get_hand_size list_players));
  ignore (List.map print_player list_players);
  list_players

(**[player_bet] returns the bet of player [player]*)
let player_bet player = player.bet

(**[has_card_of_suit] returns whether [player] has a card of suit [suit]*)
let has_card_of_suit player suit =
  List.fold_left
    (fun b x -> b || Card.get_suit x = suit)
    false player.current_hand

(**[find_first_round_trump] returns the suit trick card of
   [played_cards], meaning the first card played*)
let rec find_first_round_trump played_cards =
  match played_cards with
  | h :: t ->
      if Card.get_num h > 0 then Card.get_suit h
      else find_first_round_trump t
  | [] -> "No_Card"

(**[check_for_wizards] returns true if there are any wizard in
   [played_cards]*)
let rec check_for_wizards played_cards =
  match played_cards with
  | h :: t -> if Card.get_num h = 14 then true else check_for_wizards t
  | [] -> false

(**[print_trump] prints the trump card [trump]*)
let print_trump trump =
  ANSITerminal.print_string [ ANSITerminal.white; Bold ] "TRUMP CARD: ";
  print_cards_with_colors_short [ trump ]

(**[print_player_cards] prints the cards played in [acc]*)
let print_played_cards acc trump =
  save_cursor ();
  set_cursor 50 18;
  print_trump trump;
  let shift_line = if Card.get_num trump > 9 then -19 else -18 in
  move_cursor shift_line 2;
  ANSITerminal.print_string
    [ ANSITerminal.white; Bold ]
    "PLAYED CARDS: ";
  print_cards_with_colors_short acc;
  restore_cursor ()

(** [get_percentage] returns the percent of cards that card [card] is
    better than*)
let get_percentage player trump calc card =
  trunc
    (100.0 *. 100.0
    *. (1.
       -. Calculator.odds_of_card_winning card trump
            (Calculator.get_unplayed calc)
            (Card.make_card_list player.current_hand
               (get_hand_size player.current_hand))))
  /. 100.0

(**[choose_card_normal] returns the player [player] with their selected
   card*)
let rec choose_card_normal
    played_cards
    calc
    trump
    (player : t)
    (played_cards : Card.card list) =
  ANSITerminal.erase Screen;
  print_played_cards played_cards trump;
  print_player player;
  print_endline "\n";
  (* print_trump trump; *)
  ANSITerminal.print_string
    [ ANSITerminal.yellow; Bold ]
    "Percentage of unplayed cards strictly worse than current card: ";
  let percentage =
    get_percentage player trump calc player.current_selected_card
  in
  ANSITerminal.print_string [] (string_of_float percentage ^ " \n");
  ANSITerminal.print_string [ ANSITerminal.green; Bold ] "Play a card: ";
  ANSITerminal.print_string []
    "(prev|next|select|[integer index of card])\n\n";
  ANSITerminal.print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> (player, Card.make_no_card ())
  | command -> (
      if command = "select" then
        choose_card_normal_helper played_cards calc player played_cards
          trump choose_card_normal
      else if command = prev || command = next then
        let new_selected_player = choose_card command player in
        choose_card_normal played_cards calc trump new_selected_player
          played_cards
      else
        try
          let index = int_of_string command in
          let new_player = fst (choose_card_at_index player index) in
          choose_card_normal played_cards calc trump new_player
            played_cards
        with _ ->
          print_endline "Invalid command! \n";
          choose_card_normal played_cards calc trump player played_cards
      )

(**[choose_card_normal_helper] is the helper function for
   [choose_card_normal]*)
and choose_card_normal_helper
    played_cards
    calc
    player
    played_cards
    trump
    f =
  match player with
  | { current_selected_card = card; _ } ->
      let first_suit = find_first_round_trump played_cards in
      if
        Card.get_suit card = first_suit
        || check_for_wizards played_cards = true
        || first_suit = "No_Card"
        || has_card_of_suit player first_suit = false
        || Card.get_num card = 14
        || Card.get_num card = 0
      then (remove_current_selected_card player, card)
      else (
        ANSITerminal.print_string
          [ ANSITerminal.red; Bold ]
          "\n\n\
          \ You must follow suit or play a wizard (14) or naar (0) \n\n\
          \        press enter to continue...";
        match read_line () with
        | _ ->
            ANSITerminal.erase Screen;
            f played_cards calc trump player played_cards)

(**[valid_robot_card] returns whether the card [card] is valid to play
   given the [played_cards] *)
let valid_robot_card player played_cards card =
  let first_suit = find_first_round_trump played_cards in
  if
    Card.get_suit card = first_suit
    || check_for_wizards played_cards = true
    || first_suit = "No_Card"
    || has_card_of_suit player first_suit = false
    || Card.get_num card = 14
    || Card.get_num card = 0
  then true
  else false

(**[compare_cards] compares the card numbers of [c1] and [c2]*)
let compare_cards c1 c2 =
  if Card.get_num c1 > Card.get_num c2 then -1
  else if Card.get_num c1 < Card.get_num c2 then 1
  else 0

(**[exists_wizard] returns true if there is a wizard in
   [player_card_lst]*)
let rec exists_wizard (player_card_lst : Card.card list) : bool =
  List.map (fun y -> Card.get_num y = 14) player_card_lst
  |> List.fold_left ( || ) false

(**[first_wizard] returns the first wizard in [plyr_card]*)
let rec first_wizard (plyr_card : Card.card list) : Card.card =
  match plyr_card with
  | h :: t -> if Card.get_num h = 14 then h else first_wizard t
  | [] ->
      failwith
        "precondition violated, need at least one tuple containing a \
         wizard card"

(**[exists_trump] returns whether there is a card of the same suit as
   [trump_card] in [player_card_lst]*)
let rec exists_trump
    (player_card_lst : Card.card list)
    (trump_card : Card.card) : bool =
  List.map
    (fun y ->
      if
        Card.get_suit y = Card.get_suit trump_card && Card.get_num y > 0
      then true
      else false)
    player_card_lst
  |> List.fold_left ( || ) false

(**[first_trump] returns the first card of trump suit determined by
   [trump] in [plyr_card]*)
let rec first_trump (plyr_card : Card.card list) (trump : Card.card) :
    Card.card =
  match plyr_card with
  | h :: t ->
      if Card.get_suit h = Card.get_suit trump && Card.get_num h > 0
      then h
      else first_trump t trump
  | [] ->
      failwith
        "precondition violated, need at least one tuple containing a \
         trump card"

(**[all_zeros] returns whether [player_card_lst] has all zeros*)
let rec all_zeros (player_card_lst : Card.card list) : bool =
  match player_card_lst with
  | h :: t -> if Card.get_num h <> 0 then false else all_zeros t
  | [] -> true

(**[find_first_nonzero_card] returns the first nonzero card in
   [tuple_list]*)
let rec find_first_nonzero_card tuple_list =
  match tuple_list with
  | h :: t ->
      if Card.get_num h > 0 then h else find_first_nonzero_card t
  | [] ->
      failwith "impossible to fail, somehow did not find non-zero card"

(**[find_current_winning_card] finds the current winning card in
   [card_list] given trump card [trump]*)
let find_current_winning_card trump card_list =
  let card_list = List.rev card_list in
  let sorted_list = List.sort compare_cards card_list in
  if exists_wizard card_list then first_wizard card_list
  else if exists_trump sorted_list trump then
    first_trump sorted_list trump
  else if all_zeros card_list then List.nth card_list 0
  else
    let secondary_trump = find_first_nonzero_card card_list in
    first_trump sorted_list secondary_trump

(**[card_wins] returns true if the card [card] wins given trump card
   [trump] and played cards [played_cards]*)
let card_wins trump played_cards card =
  let card_list = card :: played_cards in
  let winning_card = find_current_winning_card trump card_list in
  winning_card = card

(**[print_basic_card_stats] prints the basic card stats of card [h]*)
let print_basic_card_stats
    player
    percentage
    trump
    played_cards
    h
    prev_percentage
    (print : bool) =
  if valid_robot_card player played_cards h && print then (
    print_endline
      ("card wins: " ^ string_of_bool (card_wins trump played_cards h));
    print_endline ("percentage: " ^ string_of_float percentage);
    print_endline
      ("card passed number, suit: "
      ^ string_of_int (Card.get_num h)
      ^ Card.get_suit h);
    print_endline ("card percentage: " ^ string_of_float percentage);
    print_endline
      ("percentage to beat: " ^ string_of_float prev_percentage))
  else ()

(**[choose_best_card_helper] is the helper function for
   [choose_best_card]*)
let rec choose_best_card_helper
    played_cards
    (player : t)
    (player_list : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    best_index
    curr_index
    best_percentage =
  match player_list with
  | [] -> best_index
  | h :: t ->
      let percentage = get_percentage player trump calc h in
      print_basic_card_stats player percentage trump played_cards h
        best_percentage false;
      if
        percentage > best_percentage
        && valid_robot_card player played_cards h
      then
        choose_best_card_helper played_cards player t calc trump
          curr_index (curr_index + 1) percentage
      else
        choose_best_card_helper played_cards player t calc trump
          best_index (curr_index + 1) best_percentage

(**[choose_best_card] chooses the card with the highest chance of
   winning based on [trump] and [played_cards]*)
let choose_best_card
    (played_cards : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    (player : t) =
  let player_hand = player.current_hand in
  let best_index =
    choose_best_card_helper played_cards player player_hand calc trump
      (-1) 0 (-1.)
  in
  fst (choose_card_at_index player best_index)

(**[choose_worst_card_helper] is the helper function for
   [choose_worst_card]*)
let rec choose_worst_card_helper
    played_cards
    (player : t)
    (player_list : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    best_index
    curr_index
    worst_percentage =
  match player_list with
  | [] -> best_index
  | h :: t ->
      let percentage = get_percentage player trump calc h in
      print_basic_card_stats player percentage trump played_cards h
        worst_percentage false;
      if
        percentage < worst_percentage
        && valid_robot_card player played_cards h
      then
        choose_worst_card_helper played_cards player t calc trump
          curr_index (curr_index + 1) percentage
      else
        choose_worst_card_helper played_cards player t calc trump
          best_index (curr_index + 1) worst_percentage

(**[choose_worst_card] chooses the card with the lowest chance of
   winning based on [trump] and [played_cards]*)
let choose_worst_card
    (played_cards : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    (player : t) =
  let player_hand = player.current_hand in
  let best_index =
    choose_worst_card_helper played_cards player player_hand calc trump
      (-1) 0 101.
  in
  fst (choose_card_at_index player best_index)

(**[choose_best_losing_card_helper] is the helper function for
   [choose_best_losing_card]*)
let rec choose_best_losing_card_helper
    played_cards
    (player : t)
    (player_list : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    best_index
    curr_index
    best_percentage =
  match player_list with
  | [] -> best_index
  | h :: t ->
      let percentage = get_percentage player trump calc h in
      print_basic_card_stats player percentage trump played_cards h
        best_percentage false;
      if
        percentage > best_percentage
        && valid_robot_card player played_cards h
        && not (card_wins trump played_cards h)
      then
        choose_best_losing_card_helper played_cards player t calc trump
          curr_index (curr_index + 1) percentage
      else
        choose_best_losing_card_helper played_cards player t calc trump
          best_index (curr_index + 1) best_percentage

(**[choose_best_losing_card] chooses the card with the highest chance of
   winning that still loses based on [trump] and [played_cards]*)
let choose_best_losing_card played_cards calc trump player =
  let player_hand = player.current_hand in
  let best_index =
    choose_best_losing_card_helper played_cards player player_hand calc
      trump (-1) 0 (-1.)
  in
  fst (choose_card_at_index player best_index)

(**[choose_best_winning_card_helper] is the helper function for
   [choose_best_winning_card]*)
let rec choose_best_winning_card_helper
    played_cards
    (player : t)
    (player_list : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    best_index
    curr_index
    best_percentage =
  match player_list with
  | [] -> best_index
  | h :: t ->
      let percentage = get_percentage player trump calc h in
      print_basic_card_stats player percentage trump played_cards h
        best_percentage false;
      if
        percentage > best_percentage
        && valid_robot_card player played_cards h
        && card_wins trump played_cards h
      then
        choose_best_winning_card_helper played_cards player t calc trump
          curr_index (curr_index + 1) percentage
      else
        choose_best_winning_card_helper played_cards player t calc trump
          best_index (curr_index + 1) best_percentage

(**[choose_best_winning_card] chooses the card with the highest chance
   of winning that still wins based on [trump] and [played_cards]*)
let choose_best_winning_card played_cards calc trump player =
  let player_hand = player.current_hand in
  let best_index =
    choose_best_winning_card_helper played_cards player player_hand calc
      trump (-1) 0 (-1.)
  in
  fst (choose_card_at_index player best_index)

(**[choose_worst_winning_card_helper] is the helper function for
   [choose_worst_winning_card]*)
let rec choose_worst_winning_card_helper
    played_cards
    (player : t)
    (player_list : Card.card list)
    (calc : Calculator.t)
    (trump : Card.card)
    best_index
    curr_index
    worst_percentage =
  match player_list with
  | [] -> best_index
  | h :: t ->
      let percentage = get_percentage player trump calc h in
      print_basic_card_stats player percentage trump played_cards h
        worst_percentage false;
      if
        percentage < worst_percentage
        && valid_robot_card player played_cards h
        && card_wins trump played_cards h
      then
        choose_worst_winning_card_helper played_cards player t calc
          trump curr_index (curr_index + 1) percentage
      else
        choose_worst_winning_card_helper played_cards player t calc
          trump best_index (curr_index + 1) worst_percentage

(**[choose_worst_winning_card] chooses the card with the lowest chance
   of winning that still wins based on [trump] and [played_cards]*)
let choose_worst_winning_card played_cards calc trump player =
  let player_hand = player.current_hand in
  let best_index =
    choose_worst_winning_card_helper played_cards player player_hand
      calc trump (-1) 0 101.
  in
  fst (choose_card_at_index player best_index)

(**[no_winning_cards] returns true if the player [player] does not have
   any winning cards in their hand [card_list] based on [played_cards]
   and [trump]*)
let rec no_winning_cards
    trump
    (card_list : Card.card list)
    played_cards
    player : bool =
  match card_list with
  | h :: t ->
      if
        card_wins trump played_cards h
        && valid_robot_card player played_cards h
      then false
      else no_winning_cards trump t played_cards player
  | [] -> true

(**[no_losing_cards] returns true if the player [player] does not have
   any losing cards in their hand [card_list] based on [played_cards]
   and [trump]*)
let rec no_losing_cards
    trump
    (card_list : Card.card list)
    played_cards
    player : bool =
  match card_list with
  | h :: t ->
      if
        (not (card_wins trump played_cards h))
        && valid_robot_card player played_cards h
      then false
      else no_losing_cards trump t played_cards player
  | [] -> true

(**[robot_needs_wins] returns true if robot [player] has bet more tricks
   than they have currently won*)
let robot_needs_wins player : bool =
  if player.bet > player.tricks_won_this_round then true else false

(**[rng_true] returns true everey [num]/10 times based on a random
   number generator*)
let rng_true num =
  let output = Random.int 10 in
  num >= output

(**[robot_decision_making] determines which type of card the robot
   [player] plays based on [played_cards] and [trump], where the robot
   uses a decision tree to determine which action they will take with
   some randomness involved at some terminal nodes*)
let robot_decision_making_choose played_cards calc trump player =
  let player_cards = player.current_hand in
  if robot_needs_wins player then
    if no_winning_cards trump player_cards played_cards player then
      choose_worst_card played_cards calc trump player
    else if rng_true 7 then
      choose_worst_winning_card played_cards calc trump player
    else choose_best_winning_card played_cards calc trump player
  else if no_losing_cards trump player_cards played_cards player then
    if rng_true 7 then choose_best_card played_cards calc trump player
    else choose_worst_winning_card played_cards calc trump player
  else choose_best_losing_card played_cards calc trump player

(**[choose_card_robot] returns a robot [player] with a selected card
   based on [trump], [played_cards], and the cards in the hand of
   [player]*)
let rec choose_card_robot
    calc
    trump
    (player : t)
    (played_cards : Card.card list) =
  ANSITerminal.erase Screen;
  (* print_trump trump; *)
  print_played_cards played_cards trump;
  print_player player;
  restore_cursor ();
  move_cursor 0 2;
  ANSITerminal.print_string
    [ ANSITerminal.yellow; Bold ]
    "Robot choosing card... press enter.\n";
  let new_robot_player =
    robot_decision_making_choose played_cards calc trump player
  in
  match read_line () with
  | _ ->
      let card = new_robot_player.current_selected_card in
      (remove_current_selected_card new_robot_player, card)

(** [choose_card_robot_human] calls the appropriate function to choose a
    card for a robot or a human for player [player]*)
let choose_card_robot_human
    played_cards
    calc
    trump
    (player : t)
    (played_cards : Card.card list) =
  if player.is_robot then
    choose_card_robot calc trump player played_cards
  else choose_card_normal played_cards calc trump player played_cards

(**[get_player_hand_list] gets the list of cards held by player [player]*)
let get_player_hand_list player = player.current_hand

(**[display_player] prints all of the information of player [player]*)
let display_player (player : t) : unit =
  match player with
  | {
   bet = b;
   tricks_won_this_round = t;
   current_score = c;
   current_hand = ch;
   current_selected_card = cc;
   current_selected_index = csi;
   player_id = pi;
   _;
  } ->
      PrintFunct.print_hand ch 0;
      move_bol ();
      save_cursor ();
      ANSITerminal.print_string
        [ ANSITerminal.magenta; Underlined; Bold ]
        ("●○●○●○●○● Player " ^ string_of_int pi
       ^ " ●○●○●○●○●");
      move_cursor 1 0;
      ANSITerminal.print_string [] ("Current bet: " ^ string_of_int b);
      move_cursor 1 0;
      ANSITerminal.print_string []
        ("Tricks won this round: " ^ string_of_int t);
      move_cursor 1 0;
      ANSITerminal.print_string [] ("Current score: " ^ string_of_int c);
      move_cursor 1 0;
      ANSITerminal.print_string [] "Current hand: ";
      move_cursor 1 0;
      print_cards_with_colors_short ch;

      print_endline "Currently selected card: ";
      print_cards_with_colors_short [ cc ];
      move_cursor 1 0;
      ANSITerminal.print_string
        [ ANSITerminal.magenta; Underlined; Bold ]
        "●○●○●○●○●○●○●○●○●○●○●○●○●○●○";
      restore_cursor ()
