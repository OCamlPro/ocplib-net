(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* The type of an UDP socket. *)
type 'info t

(* The kind of events that can be received for this socket type. *)
type event = NetTypes.udpSocketEvent

(* The type of the event handler for this socket type. *)
type 'info handler = 'info t -> event -> unit





(* This exception will be raised if the size of packets to write
  exceeds the maximal size.*)
exception PacketWriteOverflow of int * int * int

val create : ?handler:'info handler -> 'info -> Sockaddr.t -> 'info t

(* Closes the given socket. *)
val close : 'info t -> NetTypes.close_reason -> unit

(* Reads one packet from the read buffer. Can raise Queue.empty. The
  oldest packet is returned first. *)
val read : 'info t -> NetTypes.message

(* Reads all packets from the read buffer, calling the given function
on each of them in the order in which they have been received. *)
val read_packets : 'info t -> (NetTypes.message -> unit) -> unit

(* Adds a packet to the write buffer. Can raise PacketWriteOverflow. *)
val write : 'info t -> string -> Unix.sockaddr -> unit


(* Replaces the current even handler for this socket.
   The most interesting events are:
   * [`READ_DONE] : a packet was received
   * [`CAN_REFILL] : a packet was sent, there is room for another packet
   * [`WRITE_DONE] : all packets were sent. the queue is empty.
   * [`CLOSED reason] : the socket was closed locally.
 *)
val set_handler : 'info t -> 'info handler -> unit


(* Returns the number of packet currently queued in the write buffer. *)
val wlength : 'info t -> int

(* Returns the info associated with a socket. *)
val info : 'info t -> 'info

(* Should only be called after event `ACCEPTING *)
val sockaddr : 'info t -> Unix.sockaddr

(* Returns the number of packets currently queued in the read buffer. *)
val rlength : 'info t -> int

(* Converts an event to a string for human display *)
val string_of_event : NetTypes.udpSocketEvent -> string

(* Returns the current even handler for this socket. *)
val handler : 'info t -> 'info handler
