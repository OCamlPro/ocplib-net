open NetTypes

type t
type event = tcpSocketEvent
type handler = t -> event -> unit


val create : name:string ->
             ?max_buf_size: int ->
             Lwt_unix.file_descr -> handler -> t
val connect: string -> Unix.file_descr -> int -> handler -> t

val write : t -> string -> pos:int -> len:int -> unit
val write_string : t -> string -> unit

val close : t -> close_reason -> unit
val shutdown : t -> close_reason -> unit

val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit

val set_handler : t -> handler -> unit

val handler : t -> handler
val closed : t -> bool

val string_of_event : NetTypes.tcpSocketEvent -> string

val read_string : t -> string
val rlength : t -> int
val wlength : t -> int
val get : t -> int -> char
val release_bytes : t -> int -> unit
