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

(** [initialize_player a] initializes all feilds of a player object a. *)
val initialize_player : int -> t

(** [reset_round_player a] resets all necessary parts of a player object
    a. *)
val reset_round_player : t -> t

(** [choose_card a] is the function that will output the card we are
    currently looking at a = 0 or 1 for moving between cards. Raises
    NotValidMovement if the number is not 0 or 1*)
val choose_card : t -> int -> t

(** [play_card] allows a player to play the current chosen card. Returns
    a tuple of the updated player and the played card *)
val play_card : t -> t * Card.card

(** [win_trick ] allows this player to win a trick and returns the
    current state of the player *)
val win_trick : t -> t

(** [finish_round ] Finishes the round and adds trick points to the
    current players score *)
val finish_round : t -> t
