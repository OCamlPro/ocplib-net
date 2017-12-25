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

let debug = false

type event = tcpServerEvent

type 'info handler = 'info t -> event -> unit

 and 'info t = {
     mutable sock : socket;

     name : string;
     info : 'info;

     mutable last_read : float;
     mutable rtimeout : float;
     mutable sockaddr : Unix.sockaddr;
     mutable handler : 'info handler;
     mutable nconnections : int;
     events : event Queue.t;
     mutable wakener : unit Lwt.u option;
   }

let exec_handler t event =
  try
    t.handler t event
  with exn ->
    Printf.eprintf "TcpServerSocket: exception %S in handler\n%!"
                   (Printexc.to_string exn)


let activate_thread t =
  match t.wakener with
  | None -> () (* Not useful, the thread is already awake *)
  | Some u ->
     Printf.eprintf "wakeup\n%!";
     NetLoop.wakeup u;
     t.wakener <- None


(* val close : t -> close_reason -> unit *)
let close t reason =
  match t.sock with
  | Closed _ -> ()
  | Closing (_fd, _reason) -> ()
  | Socket fd ->
     t.sock <- Closing (fd, reason);
     activate_thread t

(* val closed : t -> bool *)
let closed t =
  match t.sock with
  | Socket _ -> false
  | Closing _
  | Closed _ -> true

(* val handler : t -> handler *)
let handler t = t.handler
(* val set_handler : t -> handler -> unit *)
let set_handler t h = t.handler <- h

let info t = t.info


let queue_event t event =
  Queue.add event t.events

let exec_events t =
  while not (Queue.is_empty t.events) do
    let event = Queue.take t.events in
    exec_handler t event;
  done



let rec iter_accept t =
  t.wakener <- None;
  match t.sock with
  | Closing (fd, reason) ->
     t.sock <- Closed reason;
     let on_close () =
       exec_handler t (`CLOSED reason);
       Lwt.return_unit
     in
     Lwt.catch
       (fun () ->
         Lwt.bind (Lwt_unix.close fd) on_close
       )
       (fun exn -> on_close ())

  | Closed _ -> Lwt.return_unit

  | Socket fd ->
     (* Printf.eprintf "\titer_accept...\n%!"; *)
     let accept_thread =
       Lwt.bind (Lwt_unix.accept fd)
                (fun (fd, sock_addr) ->
                  t.last_read <- NetTimer.current_time ();
                  if debug then
                    Printf.eprintf "\tServer received connection...\n%!";
                  t.nconnections <- t.nconnections + 1;
                  queue_event t (`CONNECTION (fd, sock_addr));
                  Lwt.return_unit
                )
     in
     let read_threads =
       let delay = t.rtimeout -.
                     (NetTimer.current_time () -. t.last_read)
       in
       let delay = min delay 1. in
       (* Printf.eprintf "delay: %.1f\n%!" delay; *)
       let delay = if delay < 0.01 then 0.01 else delay in
       Lwt.catch (fun () -> Lwt_unix.timeout delay)
                 (function
                  | Lwt_unix.Timeout ->
                     let time = NetTimer.current_time() in
                     if t.last_read +. t.rtimeout < time then begin
                         queue_event t `RTIMEOUT;
                         t.last_read <- time;
                       end;
                     Lwt.return_unit
                  | Lwt.Canceled ->
                     Lwt.return_unit
                  | exn ->
                     Printf.eprintf "TcpClientSocket.timeout: Exception %s\n%!"
                                    (Printexc.to_string exn);
                     Lwt.return_unit
                 )
       :: [accept_thread]
     in
     let wakener_thread, wakener_handler = Lwt.wait () in
     t.wakener <- Some wakener_handler;
     let threads = wakener_thread :: read_threads in
     Lwt.bind
       (Lwt.pick threads)
       (fun () ->
         t.wakener <- None;
         exec_events t;
         iter_accept t)

let create ?(name="unknown")
           ?(handler = (fun _t _event -> ()))
           info
           sockaddr
  =
  let domain = match sockaddr with
    | Unix.ADDR_INET _ -> Unix.PF_INET
    | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
  in
  let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec fd;
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;

  let rtimeout = -1. in
  let last_read = NetTimer.current_time () in
  let sock = Socket fd in
  let nconnections = 0 in
  let events = Queue.create () in
  let wakener = None in
  let t = {
      sock;
      name;
      sockaddr;
      info;
      handler;
      nconnections;
      last_read;
      rtimeout;
      events;
      wakener;
    } in


  let bind_socket () =
    Lwt.bind
      (Lwt_unix.bind fd sockaddr)
      (fun () ->
        Lwt_unix.listen fd 20;
        t.sockaddr <- Unix.getsockname (Lwt_unix.unix_file_descr fd);
        exec_handler t `ACCEPTING;
        iter_accept t
      )
  in
  NetLoop.defer bind_socket;
  t


let string_of_event (event : tcpServerEvent) =
  match event with
  | `CONNECTION (fd, sockaddr) ->
     Printf.sprintf "CONNECTION FROM %s"
                    (NetUtils.string_of_sockaddr sockaddr)
  | `ACCEPTING -> "ACCEPTING"
  | #NetTypes.event as event -> NetUtils.string_of_event event

(* val set_rtimeout : t -> float -> unit *)
let set_rtimeout t rtimeout = t.rtimeout <- float_of_int rtimeout

let nconnections t = t.nconnections
let sockaddr t = t.sockaddr
