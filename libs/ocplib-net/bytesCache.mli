(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

  (* [get n] gets a buffer of at least [n] bytes. The final length [m]
     such that [m >= n] will be a power of 2. *)
  val get : int -> bytes

  (* [putback b] when [b] will not be used anymore *)
  val putback : bytes -> unit
