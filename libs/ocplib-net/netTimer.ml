(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let precision = ref 0.1

let current_time = ref (Unix.gettimeofday ())

type t = {
    id : int;
    mutable next_time : float;
    mutable repeat : float option;
    action : (unit -> unit);
  }

module FloatIdMap = Map.Make(struct
                              type t = float * int
                              let compare = compare
                            end)

let counter = ref 0
let timers = ref FloatIdMap.empty

let exec_timer t =
  try t.action () with
  | exn ->
     Printf.eprintf "exec_timer: exception %s\n%!"
                    (Printexc.to_string exn)

let rec exec_timers current_time =
  if not (FloatIdMap.is_empty !timers) then
    let _, t = FloatIdMap.min_binding !timers in
    if t.next_time < current_time then begin
        timers := FloatIdMap.remove (t.next_time, t.id) !timers;
        exec_timer t;
        begin
          match t.repeat with
          | None -> ()
          | Some period ->
             t.next_time <- current_time +. period;
             timers := FloatIdMap.add (t.next_time, t.id) t !timers
        end;
        exec_timers current_time
      end


let rec iter_time () =
  Lwt.bind (Lwt_unix.sleep !precision)
           (fun () ->
             current_time := Unix.gettimeofday ();
             exec_timers !current_time;
             iter_time ())

let () = Lwt.async iter_time



let create repeat period action =
  incr counter;
  let t = {
      id = !counter;
      next_time = !current_time +. period;
      repeat = if repeat then Some period else None;
      action;
    } in
  timers := FloatIdMap.add (t.next_time, t.id) t !timers;
  t

let create_infinite period action = create true period action
let create period action = create false period action
let stop t =
  t.repeat <- None;
  timers := FloatIdMap.remove (t.next_time, t.id) !timers

let current_time () = !current_time
let set_precision p = precision := p
