(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* [NetLoop.main ()] enters the main loop, and only leaves it when the
   [NetLoop.exit ()] function is called. *)
val main : unit -> unit

(* [NetLoop.exit ()] tells the scheduler that we should exit the main loop.
   It is not guarranteed that it will be the last thing executed.
*)
val exit : unit -> unit

(* The two next functions are used to avoid running handlers within handlers.
  They defer executions of functions to the time where we come back into the
  scheduler. This way, we can always assume that the execution of the code
  within a handler is atomic.
 *)


(* defer execution of this thread to next entry into the scheduler *)
val defer : (unit -> unit Lwt.t) -> unit
(* wakeup this thread when entering the scheduler *)
val wakeup :  unit Lwt.u -> unit
