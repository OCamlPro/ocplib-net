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

val string_of_close_reason : NetTypes.close_reason -> string
val string_of_event : NetTypes.event -> string
val string_of_sockaddr : Unix.sockaddr -> string
val string_of_inet_addr : Unix.inet_addr -> string

val random_string : int -> string
