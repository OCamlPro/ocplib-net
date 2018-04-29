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

(* The type of a TCP socket to send/receive data. *)
type 'info t

(* The kind of events that can be received for this socket type. *)
(* We can only receive `RTIMEOUT or `WTIMEOUT if we are actually
   reading from or writing to the socket. Otherwise (read full or
   no write), nothing will happen until we trigger something. *)
type event = tcpSocketEvent

(* The type of the event handler for this socket type. *)
type 'info handler = 'info t -> event -> unit

(* BufferOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow of int * int * int
exception BufferReadOverflow of int * int
exception InvalidSocketOperation

(* Is it possible to miss an event if we do not provide the final event
immediately ? Is-it possible for an event to be received before the end
of create ? *)
val create : ?name:string ->
             ?max_buf_size: int ->
             ?handler: 'info handler ->
             'info ->
             Lwt_unix.file_descr ->
             'info t

val connect: ?name:string ->
             ?max_buf_size:int ->
             ?handler: 'info handler ->
             'info ->
             Sockaddr.t ->
             'info t

(* Can raise BufferWriteOverflow *)
val write_string : 'info t -> string -> pos:int -> len:int -> unit
val write_bytes : 'info t -> bytes -> pos:int -> len:int -> unit

(* Can raise BufferWriteOverflow *)
val write_string_full : 'info t -> string -> unit
val write_bytes_full : 'info t -> bytes -> unit

val close : 'info t -> NetTypes.close_reason -> unit
val shutdown : 'info t -> NetTypes.close_reason -> unit

val set_rtimeout : 'info t -> int -> unit
val set_wtimeout : 'info t -> int -> unit

val set_handler : 'info t -> 'info handler -> unit
(* specialized versions of set_handler: do not use for now. Indeed, we
have to be sure first that the we cannot miss events between
`TcpClientSocket.create` and `TcpClientSocket.set_reader`. Maybe we should
not use `Lwt.async` in `create`, but use our own async scheduler in
`NetLoop.main`.
 *)
val set_closer : 'a t -> ('a t -> NetTypes.close_reason -> unit) -> unit
val set_connected : 'a t -> ('a t -> Unix.sockaddr -> unit) -> unit
val set_reader : 'a t -> ('a t -> int -> unit) -> unit

val handler : 'info t -> 'info handler
val closed : 'info t -> bool

val string_of_event : NetTypes.tcpSocketEvent -> string

val read_all : 'info t -> string

(* Can raise BufferReadOverflow *)
val blit : 'info t -> int -> bytes -> int -> int -> unit

(* Can raise BufferReadOverflow *)
val release : 'info t -> int -> unit

(* Can raise BufferReadOverflow *)
val read : 'info t -> bytes -> int -> int -> unit

(* Can raise BufferReadOverflow *)
val read_string : 'info t -> int -> string

val max_refill : 'info t -> int
val rlength : 'info t -> int
val wlength : 'info t -> int
val get : 'info t -> int -> char
val release : 'info t -> int -> unit
val info : 'info t -> 'info
val nread : 'info t -> int
val nwritten : 'info t -> int
val set_max_output_buffer : 'info t -> int -> unit

(* can raise InvalidSocketOperation if called before being CONNECTED. Can
  be called after the connection has been closed*)
val sockaddr : 'info t -> Sockaddr.t
