(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type chars
  val get_uint8 : chars -> int -> int * int
  val get_uint16 : chars -> int -> int * int

  val get_int16 : chars -> int -> int * int

  val get_int31 : chars -> int -> int * int
  val get_int32 : chars -> int -> int32 * int

  val get_string8 : chars -> int -> string * int
  val get_string16 : chars -> int -> string * int
  val get_string31 : chars -> int -> string * int
end

module String : S with type chars := string
module Bytes : S with type chars := bytes

val set_int31 : bytes -> int -> int -> unit

val buf_int8 : Buffer.t -> int -> unit
val buf_int16 : Buffer.t -> int -> unit
val buf_int31 : Buffer.t -> int -> unit
val buf_int32 : Buffer.t -> int32 -> unit

val buf_string8 : Buffer.t -> string -> unit
val buf_string16 : Buffer.t -> string -> unit
val buf_string31 : Buffer.t -> string -> unit
