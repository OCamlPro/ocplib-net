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

(* The type of a TCP socket for a server. *)
type 'info t

(* The kind of events that can be received for this socket type. *)
type event = tcpServerEvent

(* The type of the event handler for this socket type. *)
type 'info handler = 'info t -> event -> unit

(* [create info sock ~handler] creates a TCP socket server on the
  given address. The handler is provided optionally. The [info] is
  some information that can be stored in the server data. *)
val create :
  ?name:string -> (* for debug *)
  ?handler:'info handler ->
  'info ->
  Sockaddr.t ->
  'info t

(* [close t reason] closes the current socket *)
val close : 'info t -> NetTypes.close_reason -> unit

(* [set_handler t handler] replaces the handler on the socket. Most
  interesting events are:
  * [`ACCEPTING] : the server is accepting connections. Use [sockaddr t]
      to retrieve the port if 0 was given.
  * [`CONNECTION (fd,sockaddr)] : the server received a connection, with
      file descriptor [fd] from address [sockaddr].
  * [`RTIMEOUT] : the server received no connection for the [rtimeout] time.
*)
val set_handler : 'info t -> 'info handler -> unit

(* [set_rtimeout t seconds] sets the read timeout, infinite by default. *)
val set_rtimeout : 'info t -> int -> unit

val handler : 'info t -> 'info handler
val closed : 'info t -> bool
val nconnections : 'info t -> int

(* Should only be called after event `ACCEPTING *)
val sockaddr : 'info t -> Sockaddr.t

val info : 'info t -> 'info

val string_of_event : event -> string
