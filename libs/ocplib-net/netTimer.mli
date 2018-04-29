(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* A timer with precision about 0.1s. *)
type t

(* [create delay action] creates a timer that will run [action] once
   after waiting [delay] seconds. *)
val create : ?name:string -> float -> (unit -> unit) -> t

(* [create_infinite period action] creates a timer that will run [action]
   every [period] seconds (with a minimum of 0.1s) *)
val create_infinite : ?name:string -> float -> (unit -> unit) -> t

(* [stop timer] will prevent any re-execution of the timer action. Timer
   is said to be inactive.*)
val stop : t -> unit

(* [restart timer] re-activates a timer. If the timer was already active,
   the period to wait is restarted from the current time. *)
val restart : t -> unit

(* [active timer] returns whether a timer is active or not *)
val active : t -> bool

(* Time changes at some interval. The interval is controlled by
  [set_precision] (in seconds) and defaults to 0.1 second.
   Active timers are executed after an interval if their period
   is passed. [current_time] is much more efficient than
   [gettimeofday], but less precise (but usually enough for most
   network applications).
 *)
val current_time : unit -> float
val set_precision : float -> unit
