type t = {
  bet : int;  (** This player's current bet for this trick. *)
  tricks_won_this_round : int;
      (** The number of tricks this player has already won. *)
  current_score : int;
      (** The number of rounds this player has already won. *)
  current_hand : Card.card list;
      (** The number of rounds this player has already won. *)
  current_selected_card : Card.card;
      (** The card we are focused on (current selection)*)
  current_selected_index : int;  (** Index of currently selected card*)

  (* The total score the player has accross rounds *)
  t_score : int;
}

exception OutOfBounds

exception NotValidSelection

(** [initialize_player a] initializes all feilds of a player object a. *)
let initialize_player =
  {
    bet = 0;
    tricks_won_this_round = 0;
    current_score = 0;
    current_hand = [];
    current_selected_card = Card.make_no_card ();
    current_selected_index = 0;
    t_score = 0;
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
  }

(** [find_card_at_index a b] returns the card object in the list at the
    given index Raises OutOfBounds if the index is outside the list's
    capacity*)
let rec find_card_at_index card_list index =
  match index with
  | 0 -> ( match card_list with [] -> raise OutOfBounds | h :: t -> h)
  | _ -> (
      match card_list with
      | [] -> raise OutOfBounds
      | h :: t -> find_card_at_index t (index - 1))

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

(** [choose_card a] is the function that will output the card we are
    currently looking at a = 0 or 1 for moving between cards. Raises
    NotValidMovement if the number is not 0 or 1 *)
let choose_card (player : t) move =
  if move = 0 || move = 1 then
    if move = 1 then select_previous_card player
    else select_next_card player
  else raise NotValidSelection

let rec remove_index player_list index =
  match index with
  | 0 -> (
      match player_list with [] -> raise OutOfBounds | h :: t -> t)
  | _ -> (
      match player_list with
      | [] -> raise OutOfBounds
      | h :: t -> h :: remove_index t (index - 1))

let remove_current_selected_card (player : t) =
  let curr_hand = player.current_hand in
  let curr_index = player.current_selected_index in
  {
    player with
    current_hand = remove_index curr_hand curr_index;
    current_selected_index = curr_index - 1;
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
  let curr_tricks = player.tricks_won_this_round + 1 in
  { player with tricks_won_this_round = curr_tricks }

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
