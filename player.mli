(** Representation of static player data.

    This module represents the data stored in player files, including
    the cards the player holds and the current player's bet. It handles
    all information that the player holds. *)

(** The abstract type of values representing player. *)
type t
(** type t = { *)

(* ________________________ *)

(* bet; *)

(* tricks_won_this_round; *)

(* current_score; *)

(* current_hand; *)

(* current_selected_card; *)

(* current_selected_index; *)

(* player_id; *)
(* ________________________ *)

(* The indexing is out of bounds*)
exception OutOfBounds

(** If player is trying to select a card out of bounds*)
exception NotValidSelection

exception NoCardsLeft

(** [initialize_player a] initializes all feilds of a player object a. *)
val initialize_player : int -> t

(** [reset_round_player a] resets all necessary parts of a player object
    a. *)
val reset_round_player : t -> t

(** [choose_card a] is the function that will output the card we are
    currently looking at a = 0 or 1 for moving between cards. Raises
    NotValidMovement if the number is not 0 or 1*)
val choose_card : string -> t -> t

val choose_card_rec : t -> t
(** [play_card] allows a player to play the current chosen card. Returns
    a tuple of the updated player and the played card *)
val play_card : t -> t * Card.card

(** [win_trick ] allows this player to win a trick and returns the
    current state of the player *)
val win_trick : t -> t

(** [finish_round ] Finishes the round and adds trick points to the
    current players score *)
val finish_round : t -> t

(** [make_bet t] Returns a player with t's previous stats and a new bet
    feild*)
val make_bet : int -> t -> t

(** [give_cards lst t] Returns a player with t's previous stats and a
    new hand of cards lst*)
val give_cards : Card.card list -> t -> t

(** [player_to_array ] Returns the player as a list for testing*)
val player_to_list : t -> int list

val get_player_hand : t -> string

(** [player_to_string ] Returns the player as a legible string for
    testing*)
val player_to_string : t -> string

(** [print_player] prints the player*)
val print_player : t -> unit

val player_score : t -> int

val player_id : t -> int