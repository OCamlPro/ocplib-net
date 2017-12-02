open NetTypes

type t
type event = tcpSocketEvent
type handler = t -> event -> unit


val create : name:string -> Unix.file_descr -> handler -> t
val connect: string -> Unix.file_descr -> int -> handler -> t

val write : t -> string -> pos:int -> len:int -> unit

val close : t -> close_reason -> unit
val shutdown : t -> close_reason -> unit

val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit
val set_lifetime : t -> float -> unit
val set_handler : t -> handler -> unit

val handler : t -> handler
val closed : t -> bool
