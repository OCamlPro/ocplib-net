(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open NetTypes

let () =

  let t = TcpClientSocket.connect
            ()
            (Sockaddr.of_ip "127.0.0.1" 40_000)
            ~handler:(fun t event ->
              Printf.eprintf "client: event %s\n%!"
                             (TcpClientSocket.string_of_event event);
              match event with
              | `RTIMEOUT
                | `WTIMEOUT -> ()
              | `READ_DONE _nread ->
                 let s = TcpClientSocket.read_all t in
                 Printf.printf "Received: %S\n%!" s;
                 TcpClientSocket.close t Closed_by_user
              | `CONNECTED -> ()
              | `CAN_REFILL -> ()
              | `WRITE_DONE -> ()
              | `CLOSED _reason ->
                 exit 0
            )
  in
  TcpClientSocket.write_string_full t "Hello!!!\r\n";
  Printf.eprintf "Client started\n%!";
  exit (NetLoop.main ())
