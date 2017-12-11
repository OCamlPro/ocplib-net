
open NetTypes

val string_of_close_reason : NetTypes.close_reason -> string
val string_of_event : NetTypes.event -> string
val string_of_sockaddr : Unix.sockaddr -> string
val string_of_inet_addr : Unix.inet_addr -> string

val random_string : int -> string
