(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Test with in several terminals:
╰─➤ cat /dev/zero | netcat 127.0.0.1 40000
 *)

open NetTypes

let () =
  let nconnections = ref 0 in
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, 40_000) in
  let t = TcpServerSocket.create
             ~name:"server" () sockaddr
             ~handler:(fun t event ->
               Printf.eprintf "server: event %s\n%!"
                              (TcpServerSocket.string_of_event event);
               match event with
               | `CONNECTION (fd, addr) ->
                  incr nconnections;
                  if !nconnections = 6 then
                    TcpServerSocket.close t Closed_by_user
                  else
                    let id = !nconnections in
                    let counter = ref 0 in
                    let t = TcpClientSocket.create
                              ~name:"client" () fd
                              ~handler:(fun t event ->
                                Printf.eprintf "(%d) client: event %s\n%!"
                                               id
                                               (TcpClientSocket.string_of_event event);
                                match event with
                                | `READ_DONE n ->
                                   (* Printf.eprintf "write_string ...\n%!"; *)
                                   counter := !counter + n;
                                   TcpClientSocket.write_string
                                     t
                                     (Printf.sprintf "READ %d\n" !counter);
                                   (* Printf.eprintf "read_string ...\n%!";*)
                                   let _s = TcpClientSocket.read_all t in
                                   (* Printf.eprintf "read_string DONE ...\n%!"; *)
                                   ()
                                | `RTIMEOUT ->
                                   TcpClientSocket.close t Closed_for_timeout
                                | _ -> ()
                              )
                    in
                    TcpClientSocket.set_rtimeout t 5;
                    ()
               | `CLOSED _reason -> exit 0
               | `ACCEPTING ->
                  begin
                    match TcpServerSocket.sockaddr t with
                    | Unix.ADDR_INET (_,port) ->
                       Printf.eprintf "Server started on port %d\n%!" port
                    | Unix.ADDR_UNIX file ->
                       Printf.eprintf "Server started on file %S\n%!" file
                  end
               | _ -> ()
             )
  in
  TcpServerSocket.set_rtimeout t 10;
  NetLoop.main ()
