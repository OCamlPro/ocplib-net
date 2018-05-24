(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open NetP2P
open OcpHex

type peer_info = {
    mutable peer_conn : peer_info connection option;
    mutable peer_id : string;
    mutable peer_addr : (string * int) option;
(*    ... complete with whatever you need *)
  }


module PaceMakerNetwork = NetP2P.MakeNetwork (struct
      type peer = peer_info

      let new_peer peer_id peer_addr =
        {
          peer_id = peer_id;
          peer_addr = peer_addr;
          peer_conn = None;
        }
      let peer_id p = p.peer_id
      let peer_addr p = p.peer_addr
      let peer_connection p = p.peer_conn
      let set_peer_connection p c = p.peer_conn <- c

      let protocol_name = "pace-maker"
    end)

let () = Random.self_init ()

let uid = ref (NetP2P.random_peer_id ())
let port = ref 40_000

let () =
  Arg.parse [] (fun n ->
              port := int_of_string n) "test PORT with PORT>=40000"


let () =
  PaceMakerNetwork.declare_handler PaceMakerNetwork.IdentifiedMsg.msg
    (fun p msg ->
(* This is run when a peer has just been identified *)
      Printf.printf "Connected to %s\n%!"
        (Hex.encode p.peer_id)
  );
  PaceMakerNetwork.declare_handler PaceMakerNetwork.ByeMsg.msg
    (fun p msg ->
      Printf.printf "Disconnecting from %s (%s)\n%!"
        (Hex.encode p.peer_id) (string_of_state msg.PaceMakerNetwork.ByeMsg.from_state)
  );
  PaceMakerNetwork.set_my_id  !uid;
  PaceMakerNetwork.bind_port !port;

  if !port > 40_000 then
    PaceMakerNetwork.connect_addr "127.0.0.1" 40_000;

  exit (NetLoop.main ())
