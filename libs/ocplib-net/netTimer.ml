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
  name : string option;
  mutable next_time : float;
  mutable period : float;
  action : (unit -> unit);
  mutable repeat : bool;
  mutable active : bool;
}

module FloatIdMap = Map.Make(struct
                              type t = float * int
                              let compare = compare
                            end)

let counter = ref 0
let timers = ref FloatIdMap.empty

let name t =
  match t.name with
  | None -> Printf.sprintf "(%d)" t.id
  | Some name -> name

let exec_timer t =
  try
    t.action ()
  with
  | exn ->
    NetLog.info ~exn "NetTimer.exec: exn in timer handler of %s"
      (name t)

let stop t =
  if t.active then begin
    t.active <- false;
    timers := FloatIdMap.remove  (t.next_time, t.id) !timers
  end

let active t = t.active

let restart t =
  stop t;
  t.active <- true;
  t.next_time <- !current_time +. t.period;
  timers := FloatIdMap.add (t.next_time, t.id) t !timers

let rec exec_timers current_time =
  if not (FloatIdMap.is_empty !timers) then
    let _, t = FloatIdMap.min_binding !timers in
    if t.next_time < current_time then begin
      stop t;
      if t.repeat then restart t;
      exec_timer t;
      exec_timers current_time
    end

let rec iter_time () =
  Lwt.bind (Lwt_unix.sleep !precision)
           (fun () ->
             current_time := Unix.gettimeofday ();
             exec_timers !current_time;
             iter_time ())

let () = Lwt.async iter_time

let create ?name ~repeat period action =
  if period < 0.001 then invalid_arg "NetTimer.create: delay < 0.001";
  incr counter;
  let t = {
    name;
    id = !counter;
    next_time = 0.;
    period;
    repeat;
    action = action;
    active = false;
  } in
  restart t;
  t

let create_infinite ?name period action =
  create ?name ~repeat:true period action
let create ?name period action =
  create ?name ~repeat:false period action

let current_time () = !current_time
let set_precision p = precision := p
