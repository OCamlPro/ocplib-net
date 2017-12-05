open NetTypes

type 'info t
type event = tcpServerEvent
type 'info handler = 'info t -> event -> unit

val create :
  ?name:string ->
  'info ->
  Unix.sockaddr ->
  'info handler ->
  'info t
val close : 'info t -> close_reason -> unit

val set_handler : 'info t -> 'info handler -> unit
val set_rtimeout : 'info t -> float -> unit

val handler : 'info t -> 'info handler
val closed : 'info t -> bool
val nconnections : 'info t -> int
val sockaddr : 'info t -> Unix.sockaddr
val info : 'info t -> 'info

val string_of_event : event -> string
