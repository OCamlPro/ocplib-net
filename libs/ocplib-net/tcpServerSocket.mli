open NetTypes

type 'info t
type event = tcpServerEvent
type 'info handler = 'info t -> event -> unit

val create :
  ?name:string ->
  'info ->
  Sockaddr.t ->
  'info handler ->
  'info t
val close : 'info t -> close_reason -> unit

val set_handler : 'info t -> 'info handler -> unit
val set_rtimeout : 'info t -> int -> unit

val handler : 'info t -> 'info handler
val closed : 'info t -> bool
val nconnections : 'info t -> int

(* Should only be called after event `ACCEPTING *)
val sockaddr : 'info t -> Sockaddr.t

val info : 'info t -> 'info

val string_of_event : event -> string
