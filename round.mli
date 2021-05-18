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
