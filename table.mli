(** Represents a table object that holds all the rounds and players for
    a game and runs each round.*)

(** Table type holds a round. Each round is played from table and table
    passes in the players and deck as well. *)
type t

(** [init_players a b ] returns a list of players with a real players
    and b robotic players*)
val init_players : int -> int -> Player.t list

(** [init_tb a b ] returns a table of players with a real players and b
    robotic players*)
val init_tb : int -> int -> Yojson.Basic.t -> t

(** [run_game t] returns a round object based on the given table t*)
val run_game : t -> Round.t

