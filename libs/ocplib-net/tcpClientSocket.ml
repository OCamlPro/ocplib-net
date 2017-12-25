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

(* BufferOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow = TcpBuffer.BufferWriteOverflow
exception BufferReadOverflow = TcpBuffer.BufferReadOverflow
exception InvalidSocketOperation

type event = tcpSocketEvent

type 'info t = {
    mutable sock : socket;

    name : string;
    info : 'info;

    mutable connecting : Unix.sockaddr option;
    mutable connected : Unix.sockaddr option;

    mutable handler : 'info handler;
    mutable wbuf : TcpBuffer.t;
    mutable rbuf : TcpBuffer.t;

    mutable nread : int;
    mutable last_read : float;
    mutable rtimeout : float;

    mutable nwritten : int;
    mutable last_write : float;
    mutable wtimeout : float;

    events : event Queue.t;
    mutable wakener : unit Lwt.u option;
  }

and 'info handler = 'info t -> event -> unit



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
let set_reader t f =
  let handler = t.handler in
  set_handler t
              (fun t event ->
                match event with
                | `READ_DONE nread ->
                   f t nread
                | event -> handler t event
              )
let set_closer t f =
  let handler = t.handler in
  set_handler t
              (fun t event ->
                match event with
                | `CLOSED reason ->
                   f t reason
                | event -> handler t event
              )

let sockaddr t =
  match t.connected with
  | None -> raise InvalidSocketOperation
  | Some sockaddr -> sockaddr

let set_connected t f =
  let handler = t.handler in
  set_handler t
              (fun t event ->
                match event with
                | `CONNECTED ->
                   f t (sockaddr t)
                | event -> handler t event
              )


let queue_event t event =
  Queue.add event t.events

let exec_events t =
  while not (Queue.is_empty t.events) do
    let event = Queue.take t.events in
    exec_handler t event;
    (* This is specific to TcpClientSocket *)
    match event with
    | `CAN_REFILL ->
       if TcpBuffer.length t.wbuf = 0 then
         exec_handler t `WRITE_DONE;
    | _ -> ()
  done

let rec iter_socket t =
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

  | Closed reason ->
     Lwt.return_unit
  | Socket fd ->
     let read_threads =
       match t.connecting with
       | Some sockaddr ->
          Lwt.catch
            (fun () ->
              Lwt.bind (Lwt_unix.connect fd sockaddr)
                       (fun () ->
                         t.connecting <- None;
                         t.last_read <- NetTimer.current_time ();
                         t.last_write <- NetTimer.current_time ();
                         t.connected <- Some (Lwt_unix.getpeername fd);
                         queue_event t `CONNECTED;
                         Lwt.return_unit
            ))
            (fun exn ->
              match exn with
              | Lwt.Canceled -> Lwt.return_unit
              | exn ->
                 Printf.eprintf "Exception %s in connect\n%!"
                                (Printexc.to_string exn);
                 close t (Closed_for_exception exn);
                 Lwt.return_unit
            )
          :: []
       | None ->
          begin match t.connected with
          | None ->
             t.connected <- Some (Lwt_unix.getpeername fd);
             queue_event t `CONNECTED;
          | Some _sockaddr -> ()
          end;
          if TcpBuffer.can_refill t.rbuf then
            Lwt.bind
              (TcpBuffer.add_bytes_from_read
                 t.rbuf fd
                 (min 1_000_000 (TcpBuffer.max_refill t.rbuf)))
              (fun nread ->
                (* Printf.eprintf "nread: %d\n%!" nread; *)
                if nread = 0 then
                  close t Closed_by_peer
                else
                  if nread > 0 then begin
                      t.last_read <- NetTimer.current_time ();
                      (* Printf.eprintf "last_read %.0f\n%!" t.last_read; *)
                      t.nread <- t.nread + nread;
                      queue_event t (`READ_DONE nread);
                    end;
                Lwt.return_unit
              )
            :: []
          else []
     in
     let read_threads =
       if read_threads != [] && t.rtimeout > 0. then begin
           let delay = t.rtimeout -.
                         (NetTimer.current_time () -. t.last_read)
           in
           (* Printf.eprintf "delay: %.1f\n%!" delay; *)
           let delay = if delay < 0.01 then 0.01 else delay in
           Lwt.catch (fun () -> Lwt_unix.timeout delay)
                     (function
                      | Lwt_unix.Timeout ->
                         queue_event t `RTIMEOUT;
                         t.last_read <- NetTimer.current_time ();
                         Lwt.return_unit
                      | Lwt.Canceled ->
                         Lwt.return_unit
                      | exn ->
                         Printf.eprintf "TcpClientSocket.timeout: Exception %s\n%!"
                                        (Printexc.to_string exn);
                         Lwt.return_unit
                     )
           :: read_threads
         end
       else read_threads
     in
     let write_threads =
       match t.connecting with
       | Some _sockaddr -> []
       | None ->
          if TcpBuffer.length t.wbuf > 0 then
            Lwt.bind (TcpBuffer.write t.wbuf fd)
                     (fun nwrite ->
                       if nwrite = 0 then
                         close t Closed_by_peer
                       else
                         if nwrite > 0 then begin
                             t.nwritten <- t.nwritten + nwrite;
                             queue_event t `CAN_REFILL;
                           end;
                       Lwt.return_unit
                     )
            :: []
          else []
     in
     let write_threads =
       if write_threads != [] && t.wtimeout > 0. then begin
           let delay = t.wtimeout -.
                         (NetTimer.current_time () -. t.last_write)
           in
           (* Printf.eprintf "delay: %.1f\n%!" delay; *)
           let delay = if delay < 0.01 then 0.01 else delay in
           Lwt.catch (fun () -> Lwt_unix.timeout delay)
                     (function
                      | Lwt_unix.Timeout ->
                         queue_event t `WTIMEOUT;
                         t.last_write <- NetTimer.current_time ();
                         Lwt.return_unit
                      | Lwt.Canceled -> Lwt.return_unit
                      | exn ->
                         Printf.eprintf
                           "TcpClientSocket.timeout: Exception %s\n%!"
                           (Printexc.to_string exn);
                         Lwt.return_unit
                     )
           :: write_threads
         end
       else write_threads
     in
     let wakener_thread, wakener_handler = Lwt.wait () in
     t.wakener <- Some wakener_handler;
     let threads = wakener_thread :: read_threads @ write_threads in
     Lwt.bind
       (Lwt.pick threads)
       (fun () ->
         t.wakener <- None;
         exec_events t;
         iter_socket t)

let dummy_handler t event =
  match event with
  | `RTIMEOUT | `WTIMEOUT -> close t Closed_for_timeout
  | #event -> ()

(* val create : name:string -> Unix.file_descr -> handler -> t *)
let create ?(name="unknown") ?(max_buf_size = 1_000_000)
           ?connecting
           ?(handler=dummy_handler)
           info
           fd
            =
  let sock = Socket fd in
  let rbuf = TcpBuffer.create max_buf_size in
  let wbuf = TcpBuffer.create max_buf_size in
  let rtimeout = -1. in
  let wtimeout = -1. in
  let last_read = NetTimer.current_time () in
  let last_write = NetTimer.current_time () in
  let nread = 0 in
  let nwritten = 0 in
  let connected = None in
  let events = Queue.create () in
  let wakener = None in
  let t = {
      name;
      sock;
      rbuf;
      wbuf;
      info;
      handler;
      connecting;
      connected;
      last_read;
      rtimeout;
      last_write;
      wtimeout;
      nread;
      nwritten;
      events;
      wakener;
    } in
  NetLoop.defer (fun () -> iter_socket t);
  t


(* val connect: string -> Unix.file_descr -> int -> handler -> t *)
let connect ?name ?max_buf_size ?handler info sockaddr =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  create ?name ?max_buf_size ?connecting:(Some sockaddr) ?handler info fd

let create ?name ?max_buf_size ?handler info fd  =
  create ?name ?max_buf_size info fd ?handler

(* val write : t -> string -> pos:int -> len:int -> unit *)
let write t s ~pos ~len =
  if len > 0 then begin
      if TcpBuffer.length t.wbuf = 0 then
        t.last_write <- NetTimer.current_time ();
      TcpBuffer.add_bytes_from_string t.wbuf s pos len;
      activate_thread t
    end

let write_string t s =
  write t s ~pos:0 ~len:(String.length s)

(* val shutdown : t -> close_reason -> unit *)
let shutdown t reason =
  match t.sock with
  | Closed _
    | Closing _ -> ()
  | Socket fd ->
     Lwt_unix.shutdown fd Lwt_unix.SHUTDOWN_ALL;
     close t reason

(* val set_rtimeout : t -> float -> unit *)
let set_rtimeout t rtimeout =
  t.rtimeout <- float_of_int rtimeout;
  activate_thread t

(* val set_wtimeout : t -> float -> unit *)
let set_wtimeout t wtimeout =
  t.wtimeout <- float_of_int wtimeout;
  if TcpBuffer.length t.wbuf > 0 then
    activate_thread t

(* val set_lifetime : t -> float -> unit *)
(*let set_lifetime _ = assert false *)


let string_of_event (event : tcpSocketEvent) =
  match event with
  | `CONNECTED -> "CONNECTED"
  | `READ_DONE n ->
     Printf.sprintf "READ_DONE %d" n
  | `CAN_REFILL -> "CAN_REFILL"
  | `WRITE_DONE -> "WRITE_DONE"
  | #NetTypes.event as event -> NetUtils.string_of_event event


let max_refill t = TcpBuffer.max_refill t.rbuf

let blit t pos0 s pos len =
  TcpBuffer.blit t.rbuf pos0 s pos len
let release t n =
  if n > 0 then begin
      TcpBuffer.release_bytes t.rbuf n;
      activate_thread t
    end
let read t s pos len =
  TcpBuffer.read t.rbuf s pos len;
  activate_thread t
let read_string t len =
  let s = Bytes.create len in
  read t s 0 len;
  s
let read_all t =
  read_string t (TcpBuffer.length t.rbuf)

let rlength t = TcpBuffer.length t.rbuf

let set_max_output_buffer t n = TcpBuffer.set_max_buf_size t.wbuf n
let wlength t = TcpBuffer.length t.wbuf
let get t pos = TcpBuffer.get t.rbuf pos

let nwritten t = t.nwritten
let nread t = t.nread
let info t = t.info
