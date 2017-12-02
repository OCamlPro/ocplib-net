type t

val create : int -> (unit -> unit) -> t
val stop : t -> unit
