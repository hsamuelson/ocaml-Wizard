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
  player_id : int; (* the id of the player *)
}

exception OutOfBounds

exception NoCardsLeft

exception NotValidSelection

(** [initialize_player a] initializes all feilds of a player object a. *)
let initialize_player (p_num : int) =
  {
    bet = 0;
    tricks_won_this_round = 0;
    current_score = 0;
    current_hand = [];
    current_selected_card = Card.make_no_card ();
    current_selected_index = 0;
    player_id = p_num;
  }

(** [reset_round_player a] resets all necessary parts of a player object
    a. *)
let reset_round_player (player : t) =
  {
    player with
    bet = 0;
    tricks_won_this_round = 0;
    current_hand = [];
    current_selected_index = 0;
    current_selected_card = Card.make_no_card ();
  }

(** [find_card_at_index a b] returns the card object in the list at the
    given index Raises OutOfBounds if the index is outside the list's
    capacity*)
let rec find_card_at_index (card_list : Card.card list) index =
  match index with
  | 0 -> ( match card_list with [] -> raise NoCardsLeft | h :: t -> h)
  | _ -> (
      match card_list with
      | [] -> raise OutOfBounds
      | h :: t -> if t = [] then h else find_card_at_index t (index - 1)
      )

(** [select_previous_card a] returns the card object in the player's
    hand at the previous selected index*)
let select_previous_card (player : t) =
  match player with
  | { current_hand = hand; current_selected_index = index; _ } ->
      {
        player with
        current_selected_index = index - 1;
        current_selected_card = find_card_at_index hand (index - 1);
      }

(** [select_next_card a] returns the card object in the player's hand at
    the next selected index*)
let select_next_card (player : t) =
  match player with
  | { current_hand = hand; current_selected_index = index; _ } ->
      {
        player with
        current_selected_index = index + 1;
        current_selected_card = find_card_at_index hand (index + 1);
      }

(* Choose_card does not work on its own we need this rec funciton *)
let rec choose_card_rec (player : t) =
  print_endline "Play a card: (prev|next|select)\n";
  ANSITerminal.print_string [ Bold ] "> ";
  match read_line () with
  | exception End_of_file -> player
  | command ->
      if command = "select" then player else choose_card_rec player

(** [choose_card a] is the function that will output the card we are
    currently looking at a = 0 or 1 for moving between cards. Raises
    NotValidMovement if the number is not 0 or 1 *)
let choose_card move (player : t) =
  if move = "next" || move = "prev" then
    if move = "prev" then select_previous_card player
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

(** [get_hand_size a] returns the number of cards that are in the given
    hand a *)
let rec get_hand_size hand =
  match hand with [] -> 0 | h :: t -> 1 + get_hand_size t

(** [remove_current_selected_card] returns a new player without the card
    in hand that is currently selected. Used in play_card. If the
    selected card is 0, we keep the selected index at 0. Otherwise, we
    make the new selected card at previous_index -1*)
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

(** [play_card] allows a player to play the current chosen card. Returns
    a tuple of the updated player and the played card *)
let play_card (player : t) =
  match player with
  | { current_selected_card = card; _ } ->
      (remove_current_selected_card player, card)

(** [win_trick ] allows this player to win a trick and returns the
    current state of the player *)
let win_trick (player : t) =
  {
    player with
    tricks_won_this_round = player.tricks_won_this_round + 1;
  }

(** [finish_round ] Finishes the round and adds trick points to the
    current players score *)
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

(** [make_bet]*)
let make_bet bet (player : t) = { player with bet }

let player_score (plyr : t) = plyr.current_score

let player_id (plyr : t) = plyr.player_id

let get_card_color card_suit =
  match card_suit with
  | "red" -> ANSITerminal.red
  | "blue" -> ANSITerminal.cyan
  | "green" -> ANSITerminal.green
  | "yellow" -> ANSITerminal.yellow
  | _ -> ANSITerminal.white

let rec print_cards_with_colors card_list =
  match card_list with
  | h :: t ->
      ANSITerminal.print_string
        [ get_card_color (Card.get_suit h); Bold ]
        (Card.string_of_card h);
      print_cards_with_colors t
  | [] -> ()

let rec print_cards_with_colors_short card_list =
  match card_list with
  | h :: t ->
      ANSITerminal.print_string
        [ get_card_color (Card.get_suit h); Bold ]
        ("|" ^ string_of_int (Card.get_num h) ^ "| ");
      print_cards_with_colors_short t
  | [] -> ()

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
      ANSITerminal.erase Screen;
      print_string ("Player " ^ string_of_int pi);
      print_string (" information: \nCurrent bet: " ^ string_of_int b);
      print_string ("\nTricks won this round: " ^ string_of_int t);
      print_string ("\nCurrent score: " ^ string_of_int c);
      print_string "\nCurrent hand: ";
      print_cards_with_colors_short ch;
      print_string "\nCurrently selected card: ";
      print_cards_with_colors_short [ cc ];
      print_endline
        ("\nCurrently selected index: " ^ string_of_int csi ^ "\n")

let player_bet player = player.bet
