type close_reason =
  Closed_for_timeout    (* timeout exceeded *)
| Closed_for_lifetime   (* lifetime exceeded *)
| Closed_by_peer        (* end of file *)
| Closed_for_error of string
| Closed_by_user
| Closed_for_overflow
| Closed_connect_failed
| Closed_for_exception of exn

type socket =
  | Socket of Lwt_unix.file_descr
  | Closing of Lwt_unix.file_descr * close_reason
  | Closed of close_reason

type event =
  [
  | `CLOSED of close_reason
  | `RTIMEOUT  (* called after a timeout on reading *)
  | `WTIMEOUT  (* called after a timeout on writing *)
  ]

type tcpSocketEvent = [
  | `CONNECTED  (* we are connected ! *)
  | `CAN_REFILL (* some space if free in the buffer *)
  | `WRITE_DONE (* even after refill, nothing to send *)
  (*  | BUFFER_OVERFLOW (* ??? *) *)
  | `READ_DONE of int (* there are bytes to read in the buffer *)
  | event ]

type tcpServerEvent = [
  | `ACCEPTING
  | `CONNECTION of Lwt_unix.file_descr * Unix.sockaddr
  | event ]

type udpSocketEvent = [
  | `ACCEPTING
  | `WRITE_DONE
  | `CAN_REFILL
  | `READ_DONE
  | event ]

type message = {
    msg_content : string;
    msg_sockaddr : Unix.sockaddr;
  }
