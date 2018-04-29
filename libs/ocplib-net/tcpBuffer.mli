(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* This module is used internally by TcpClientSocket. It should not be
   used directly. *)

type t

(* BufferOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow of int * int * int
exception BufferReadOverflow of int * int

val create : int -> t
(* Release unused buffer *)
val release : t -> unit

val add_bytes : t -> bytes -> int -> int -> unit
val add_string : t -> string -> int -> int -> unit
val add_from_fd :
  t -> Lwt_unix.file_descr -> int -> int Lwt.t

(* Release a given number of bytes *)
val release_bytes : t -> int -> unit

(* Number of bytes in buffer *)
val length : t -> int

(* Maximal number of bytes that could be written *)
val max_refill : t -> int
val can_refill : t -> bool
val set_max_buf_size : t -> int -> unit

val get : t -> int -> char

(* bytes read are NOT released *)
val blit : t -> int -> bytes -> int -> int -> unit
(* bytes read are immediately released *)
val read : t -> bytes -> int -> int -> unit

val write : t -> Lwt_unix.file_descr -> int Lwt.t
