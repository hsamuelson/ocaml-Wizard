

type t

(** This player's current bet for this trick. *)
type bet = int

(** The number of tricks this player has already won. *)
type tricks_won_this_round = int

(** The number of rounds this player has already won. *)
type current_score = int

type current_hand = Card.card list

let x = print_endline "hello world"  