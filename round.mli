(** Represents a round object that handles all players and movements on
    the board.*)

(** The abstract type of values representing a round *)
type t

(** [init_first_round a b c] returns the round object of #a using the
    deck of cards b and the player list c *)
val init_first_round : int -> Card.card_list -> Player.t list -> t

(** [play_round a] plays the round object and returns the newly formed
    round*)
val play_round : t -> t

(** [scoreboard a ] returns the scores of all players as a tuple of
    player id, score*)
val scoreboard : Player.t list -> string * string

(** [players a] returns the list of players in a round*)
val players : t -> Player.t list

(** [round_num a] returns the rumber of the round a*)
val round_num : t -> int

(**[deck_size a] returns the size of the deck in round a*)
val deck_size : t -> int

(** [print_scoreboard a] prints the scoreboard associated with the round
    a at that current time*)
val print_scoreboard : t -> unit

(** [find_winning_card a b] returns the player and the players card that
    wins the round*)
val find_winning_card :
  Card.card -> (Player.t * Card.card) list -> Player.t * Card.card

(** [find_round_leader a b c] returns a list of players where the
    correct first player will be first in the list. This function
    rotates the table where the original list of players is a, the round
    number is b and the round object is c.*)
val find_round_leader : Player.t list -> int -> t -> Player.t list
(* [gen_next_round a b] returns the next round object. Change round
   object to be ready to be run on next round *)
val gen_next_round : t-> Player.t list -> t