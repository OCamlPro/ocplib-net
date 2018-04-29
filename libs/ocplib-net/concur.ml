(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open StringCompat
open NetTypes

(* A simple client-server *)

let debug = false

type 'info connection = {
  info : 'info;
  mutable fd : (('info connection) TcpClientSocket.t) option;
}

let info con = con.info

let send_message con msg =
  if debug then Printf.eprintf "send_message...\n%!";
  let msg_len = String.length msg in
  let b = Bytes.create 4 in
  EndianString.LittleEndian.set_int32 b 0 (Int32.of_int msg_len);
  match con.fd with
  | None -> assert false
  | Some fd ->
     TcpClientSocket.write_bytes_full fd b;
     TcpClientSocket.write_string_full fd msg

let shutdown con =
  match con.fd with
  | None -> assert false
  | Some fd ->
     TcpClientSocket.shutdown fd Closed_by_user

let close con =
  match con.fd with
  | None -> assert false
  | Some fd ->
     TcpClientSocket.close fd Closed_by_user


module MakeSocket(S : sig

  type server_info
  type info

  val connection_info : server_info -> Unix.sockaddr -> info

  val accepting_handler : server_info -> Unix.sockaddr -> unit
  val connection_handler : info connection -> unit
  val message_handler : info connection -> string -> unit
  val disconnection_handler : info -> unit

end) = (struct

  let disconnection_handler con =
    try
      S.disconnection_handler con
    with exn ->
      Printf.eprintf "Warning: [disconnection_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let connection_handler con =
    try
      S.connection_handler con
    with exn ->
      Printf.eprintf "Warning: [connection_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let message_handler con msg =
    try
      S.message_handler con msg
    with exn ->
      Printf.eprintf "Warning: [message_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let accepting_handler con sockaddr =
    try
      S.accepting_handler con sockaddr
    with exn ->
      Printf.eprintf "Warning: [accepting_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let update_con con fd =
    match con.fd with
    | None -> con.fd <- Some fd
    | Some _ -> ()

  let reader t event =
    if debug then
      Printf.eprintf "client event: %s\n%!"
                   (TcpClientSocket.string_of_event event);
    let con = TcpClientSocket.info t in
    update_con con t;
    match event with
    | `READ_DONE _n ->
       if TcpClientSocket.rlength t >= 4 then
         let s = Bytes.create 4 in
         TcpClientSocket.blit t 0 s 0 4;
         let msg_len = Int32.to_int
                         (EndianBytes.LittleEndian.get_int32 s 0) in
         if TcpClientSocket.rlength t >= 4 + msg_len then begin
             TcpClientSocket.release t 4;
             let s = Bytes.create msg_len in
             TcpClientSocket.read t s 0 msg_len;
             message_handler con (Bytes.to_string s)
           end
    | `CLOSED _reason ->
       disconnection_handler con.info;
    | `CONNECTED ->
       connection_handler con
    | _ -> ()

  let create ~loopback ?(port=0) context =
    let sockaddr = Unix.ADDR_INET(
                       (if loopback then
                          Unix.inet_addr_of_string "127.0.0.1"
                        else
                          Unix.inet_addr_any),
                       port) in
    let _t = TcpServerSocket.create
               ()
               sockaddr
              ~handler:(fun t event ->
                if debug then
                  Printf.eprintf "event: %s\n%!"
                                 (TcpServerSocket.string_of_event event);
                match event with
                | `CONNECTION (fd, sockaddr) ->
                   let info =
                     S.connection_info context sockaddr in
                   let con = { info; fd = None } in
                   let fd = TcpClientSocket.create
                              con
                              fd
                              ~handler:reader
                   in
                   con.fd <- Some fd
                | `ACCEPTING ->
                   accepting_handler context
                                     (TcpServerSocket.sockaddr t)
                | _ -> ()
              )
    in
    ()

  let create_server = create

  let connect info sockaddr =
    let con = { info; fd = None } in
    let fd = TcpClientSocket.connect
               con
               sockaddr
               ~handler:reader
    in
    con.fd <- Some fd;
    con

end : sig

  val create : loopback:bool -> ?port:int -> S.server_info -> unit
  val create_server : loopback:bool -> ?port:int -> S.server_info -> unit
  val connect : S.info -> Lwt_unix.sockaddr -> S.info connection

end)


(* create sockets as server *)
module MakeServer(S : sig

  type server_info
  type info

  val accepting_handler : server_info -> Unix.sockaddr -> unit

  val connection_info : server_info -> Unix.sockaddr -> info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) = MakeSocket(S)

(* create sockets as client *)
module MakeClient(S : sig

  type info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) = MakeSocket(struct
  type server_info = S.info
  include S
  let accepting_handler _server_info _sockaddr = assert false
  let connection_info _sockaddr = assert false
end
)

module Timer = NetTimer
include NetLoop
include NetCommand
