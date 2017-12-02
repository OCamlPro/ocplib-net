type t

val create : int -> (unit -> unit) -> t
val stop : t -> unit

(* time changes every second *)
val current_time : unit -> float
