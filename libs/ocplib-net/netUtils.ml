
open NetTypes

let string_of_close_reason = function
  | Closed_for_timeout -> "timeout"
  | Closed_for_lifetime -> "lifetime"
  | Closed_by_peer -> "peer"
  | Closed_for_error error -> Printf.sprintf "error %s" error
  | Closed_by_user -> "user"
  | Closed_for_overflow -> "overflow"
  | Closed_connect_failed -> "connect failed"
  | Closed_for_exception e -> Printf.sprintf "exception %s"
                                             (Printexc.to_string e)

let string_of_event (t : event) =
  match t with
  | `CLOSED s -> Printf.sprintf "CLOSED %s" (string_of_close_reason s)
  | `RTIMEOUT -> "RTIMEOUT"
  | `LTIMEOUT -> "LTIMEOUT"
  | `WTIMEOUT -> "WTIMEOUT"

let string_of_sockaddr = function
  | Unix.ADDR_INET (addr, port) ->
     Printf.sprintf "ADDR_INET(%s, %d)"
                    (Unix.string_of_inet_addr addr) port
  | Unix.ADDR_UNIX s ->
     Printf.sprintf "ADDR_UNIX %S" s