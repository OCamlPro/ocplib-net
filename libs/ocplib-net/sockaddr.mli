
type t = Unix.sockaddr

val loopback : int -> t
val any : t
val to_string : t -> string
val of_ip : string -> int -> t
