(** Representation of static player data.

    This module represents the data stored in player files, including
    the cards the player holds and the current player's bet. It handles
    all information that the player holds. *)

(** The abstract type of values representing player. *)
type t

(** The indexing is out of bounds*)
exception OutOfBounds

(** If player is trying to select a card out of bounds*)
exception NotValidSelection

(**If there are no cards left to deal out*)
exception NoCardsLeft

(** [initialize_player] initializes all fields of a player object
    [p_num], and makes the player a robot if [robot] *)
val initialize_player : int -> bool -> t

(** [reset_round_player] resets all necessary parts of a player object
    [player]. *)
val reset_round_player : t -> t

(** [choose_card] either chooses the card at the current selection that
    player [player] is holding or it selects the next or prev card based
    on [move]*)
val choose_card : string -> t -> t

(** [choose_card_robot_human] calls the appropriate function to choose a
    card for a robot or a human for player [player]*)
val choose_card_robot_human :
  Card.card list ->
  Calculator.t ->
  Card.card ->
  t ->
  Card.card list ->
  t * Card.card

(** [win_trick ] allows this player to win a trick and returns the
    current state of the player *)
val win_trick : t -> t

(** [finish_round ] Finishes the round and adds trick points to the
    current players score *)
val finish_round : t -> t

(** [make_bet t] Returns a player with t's previous stats and a new bet
    feild*)
val make_bet : int -> t -> t

val get_player_hand_list : t -> Card.card list

(** [give_cards lst t] Returns a player with t's previous stats and a
    new hand of cards lst*)
val give_cards : Card.card list -> t -> t

(** [player_to_array ] Returns the player as a list for testing*)
val player_to_list : t -> int list

val get_player_hand : t -> string

(** [player_to_string ] Returns the player as a legible string for
    testing*)
val player_to_string : t -> string

(** [print_player] prints the player using print_string (also will print
    the colors of the cards)*)
val print_player : t -> unit

(** [player_score] returns the score of the player *)
val player_score : t -> int

(** [player_id] returns the id of the player*)
val player_id : t -> int

(** [player_bet] returns the bet of the player*)
val player_bet : t -> int

(** [print_player_list] prints the list of players then returns the
    original list of players*)
val print_player_list : t list -> t list

(** [print_cards_with_colors_short] prints the cards in [card_list] with
    colors in a small format*)
val print_cards_with_colors_short : Card.card list -> unit

(** [choose_card_at_index] chooses the card at index [index] in player
    [player]'s hand*)
val choose_card_at_index : t -> int -> t * Card.card

(** [get_card_color] returns the associated color of a card suit
    [card_suit]*)
val get_card_color : string -> ANSITerminal.style

(** [is_robot] returns whether the given player [player] is a robot*)
val get_is_robot : t -> bool

(** [get_percentage] returns the percent of cards that card [card] is
    better than*)
val get_percentage :
  t -> Card.card -> Calculator.t -> Card.card -> float
