(** Representation of static player data.

    This module represents the data stored in player files, including
    the cards the player holds and the current player's bet. 
    It handles all information that the player holds. *)

(** The abstract type of values representing player. *)
type t

(** This player's current bet for this trick. *)
type bet = int

(** The number of tricks this player has already won. *)
type tricks_won_this_round = int

(** The number of rounds this player has already won. *)
type current_score = int

(** The number of rounds this player has already won. *)
type current_hand = Card.card list
(* 
(** [start_room a] is the identifier of the starting room in adventure
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string *)

