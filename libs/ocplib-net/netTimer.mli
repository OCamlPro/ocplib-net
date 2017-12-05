type t

val create : float -> (unit -> unit) -> t
val stop : t -> unit

val create_infinite : float -> (unit -> unit) -> t

(* time changes every second *)
val current_time : unit -> float
val set_precision : float -> unit
