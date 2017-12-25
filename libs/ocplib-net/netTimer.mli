(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t

val create : float -> (unit -> unit) -> t
val stop : t -> unit

val create_infinite : float -> (unit -> unit) -> t

(* time changes every second *)
val current_time : unit -> float
val set_precision : float -> unit
