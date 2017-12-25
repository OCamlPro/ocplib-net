(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(*
  Let's give a template of code:



type peer_info = {
    mutable peer_conn : peer_info connection option;
    mutable peer_id : Sha1.t;
    mutable peer_addr : (Ip.t * int) option;
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
      let peer_address p = p.peer_addr
      let peer_connection p = p.peer_conn
      let set_peer_connection p c = p.peer_conn <- c

      let protocol_name = "pace-maker"
    end)

let _ =
  PaceMakerNetwork.declare_handler PaceMakerNetwork.IdentifiedMsg.msg
    (fun p msg ->
(* This is run when a peer as just been identified *)
      Printf.printf "Connected to %s\n%!"
        (Sha1.to_string p.peer_id)
  );
  PaceMakerNetwork.declare_handler PaceMakerNetwork.ByeMsg.msg
    (fun p msg ->
      Printf.printf "Disconnecting from %s (%s)\n%!"
        (Sha1.to_string p.peer_id) (string_of_state msg.PaceMakerNetwork.ByeMsg.from_state)
  );
  PaceMakerNetwork.set_my_id  !uid;
  let _s = PaceMakerNetwork.bind_port !port in
  ()

*)

type connection_state =
  CONNECTING
| CONNECTED
| IDENTIFIED
| DISCONNECTED

val string_of_state : connection_state -> string

type ('a, 'b) message_type = {
    message_name : string;
    mutable message_writer : Buffer.t -> 'a -> unit;
    mutable message_reader : string -> int -> 'a;
    mutable message_printer : 'a -> string;
  }

type peer_ip = string
type peer_id = string

type 'a connection
(*
  = {
    mutable conn_peer : 'a option;
    mutable conn_sock : (unit TcpClientSocket.t) option;
    mutable conn_addr : peer_ip * int;
    mutable conn_state : connection_state;
  }
 *)

val random_peer_id : unit -> string

module MakeNetwork :
functor
  (M : sig
     type peer
     val peer_id : peer -> peer_id
     val new_peer : peer_id -> (peer_ip * int) option -> peer
     val peer_connection : peer -> peer connection option
     val set_peer_connection :
       peer -> peer connection option -> unit
     val peer_addr : peer -> (peer_ip * int) option
     val protocol_name : string
   end) ->
sig
  type peer = M.peer
  val declare_message :
    string ->
    (Buffer.t -> 'a -> unit) ->
    (string -> int -> 'a) ->
    ('a -> string) -> ('a, peer) message_type
  val declare_handler :
    ('a, peer) message_type -> (peer -> 'a -> unit) -> unit


  val send : peer -> ('a, peer) message_type -> 'a -> unit

  val new_peer : peer_id -> (peer_ip * int) option -> peer
  val connect : peer -> unit
  val connect_addr : peer_ip -> int -> unit
  val disconnect : peer -> unit
  val bind_port : int -> unit
  val set_my_port : int -> unit
  val set_my_id : peer_id -> unit
  val get_my_id : unit -> peer_id
  val peer_state : peer -> connection_state

  val set_rtimeout : peer -> int -> unit
  val set_max_output_buffer : peer -> int -> unit

  module IdentifiedMsg :
  sig
    type t = { ip : peer_ip; port : int; my_port : int }
    val msg : (t, 'a) message_type
  end
  module ByeMsg :
  sig
    type t = { from_state : connection_state; }
    val msg : (t, 'a) message_type
  end


  module MakeUnitMsg : functor (M : sig val name : string end) ->
                       sig type t = unit val msg : (unit, 'a) message_type end

  val iter_peers : (peer -> unit) -> unit
  val get_peer : peer_id -> peer

end
