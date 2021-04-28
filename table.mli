type t

val init_players : int -> int -> Player.t list

val init_tb : int -> int -> Yojson.Basic.t -> t

val run_game : t -> Round.t
