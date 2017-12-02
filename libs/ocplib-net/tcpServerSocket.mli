open NetTypes

type t
type event = tcpServerEvent
type handler = t -> event -> unit

val create : name:string -> ?addr:Unix.inet_addr -> ?port:int -> handler -> t
val close : t -> close_reason -> unit

val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit
val set_lifetime : t -> float -> unit
val set_handler : t -> handler -> unit

val handler : t -> handler
val closed : t -> bool
val port : t -> int

val string_of_event : event -> string
