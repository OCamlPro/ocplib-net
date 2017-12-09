
type 'info t
type event = NetTypes.udpSocketEvent
type 'info handler = 'info t -> event -> unit

exception PacketWriteOverflow of int * int * int

val handler : 'info t -> 'info handler
val set_handler : 'info t -> 'info handler -> unit

val create : 'info -> Sockaddr.t -> 'info handler -> 'info t
val close : 'info t -> NetTypes.close_reason -> unit
val info : 'info t -> 'info

(* Should only be called after event `ACCEPTING *)
val sockaddr : 'info t -> Unix.sockaddr

val rlength : 'info t -> int

(* can raise Queue.empty *)
val read : 'info t -> NetTypes.message
val read_packets : 'info t -> (NetTypes.message -> unit) -> unit

val wlength : 'info t -> int
val write : 'info t -> string -> Unix.sockaddr -> unit

val string_of_event : NetTypes.udpSocketEvent -> string
