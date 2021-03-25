type t

val init_players : int -> Player.t list

val init_tb : int -> Yojson.Basic.t -> t
