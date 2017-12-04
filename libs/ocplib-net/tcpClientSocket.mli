open NetTypes

(* BufferOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow of int * int * int
exception BufferReadOverflow of int * int

type 'info t

(* We can only receive `RTIMEOUT or `WTIMEOUT if we are actually
   reading from or writing to the socket. Otherwise (read full or
   no write), nothing will happen until we trigger something. *)
type event = tcpSocketEvent
type 'info handler = 'info t -> event -> unit


val create : ?name:string ->
             ?max_buf_size: int ->
             'info ->
             Lwt_unix.file_descr ->
             'info handler ->
             'info t
val connect: ?name:string ->
             ?max_buf_size:int ->
             'info ->
             Unix.sockaddr ->
             'info handler ->
             'info t

val write : 'info t -> string -> pos:int -> len:int -> unit
val write_string : 'info t -> string -> unit

val close : 'info t -> close_reason -> unit
val shutdown : 'info t -> close_reason -> unit

val set_rtimeout : 'info t -> float -> unit
val set_wtimeout : 'info t -> float -> unit

val set_handler : 'info t -> 'info handler -> unit

val handler : 'info t -> 'info handler
val closed : 'info t -> bool

val string_of_event : NetTypes.tcpSocketEvent -> string

val blit : 'info t -> int -> string -> int -> int -> unit
val release : 'info t -> int -> unit
val read : 'info t -> string -> int -> int -> unit
val read_string : 'info t -> string
val rlength : 'info t -> int
val wlength : 'info t -> int
val get : 'info t -> int -> char
val release_bytes : 'info t -> int -> unit
val info : 'info t -> 'info
val nread : 'info t -> int
val nwritten : 'info t -> int
