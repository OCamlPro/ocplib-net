
open NetTypes

type event = tcpSocketEvent

type t = {
    mutable sock : socket;

    name : string;

    mutable active_reader : bool;
    mutable active_writer : bool;
    mutable handler : handler;
    mutable wbuf : TcpBuffer.t;
    mutable rbuf : TcpBuffer.t;
    mutable last_read : float;
    mutable rtimeout : float;
  }

and handler = t -> event -> unit



let exec_handler t event =
  try
    t.handler t event
  with exn ->
    Printf.eprintf "TcpServerSocket: exception %S in handler\n%!"
                   (Printexc.to_string exn)

(* val close : t -> close_reason -> unit *)
let close t reason =
  match t.sock with
  | Closed _ -> ()
  | Socket fd ->
     t.sock <- Closed reason;
     Lwt.async (fun () ->
         Lwt.bind (Lwt_unix.close fd)
                  (fun () ->
                    exec_handler t (`CLOSED reason);
                    Lwt.return ()
                  ))

(* val closed : t -> bool *)
let closed t =
  match t.sock with
  | Socket _ -> false
  | Closed _ -> true

(* val handler : t -> handler *)
let handler t = t.handler
(* val set_handler : t -> handler -> unit *)
let set_handler t h = t.handler <- h





let rec iter_read_actions t =
  match t.sock with
  | Closed _ -> Lwt.return ()
  | Socket fd ->
     let threads = [] in
     let threads =
       if TcpBuffer.can_refill t.rbuf then
         Lwt.bind
           (TcpBuffer.add_bytes_from_read
              t.rbuf fd
              (min 1_000_000 (TcpBuffer.max_refill t.rbuf)))
           (fun nread ->
             (* Printf.eprintf "nread: %d\n%!" nread; *)
             if nread = 0 then
               close t Closed_by_peer
             else begin
                 t.last_read <- NetTimer.current_time ();
                 (* Printf.eprintf "last_read %.0f\n%!" t.last_read; *)
                 exec_handler t (`READ_DONE nread);
               end;
             Lwt.return ()
           )
         :: threads
       else threads
     in
     let threads =
       if t.rtimeout > 0. then begin
           let delay = t.rtimeout -.
                         (NetTimer.current_time () -. t.last_read)
           in
           (* Printf.eprintf "delay: %.1f\n%!" delay; *)
           let delay = if delay < 0.01 then 0.01 else delay in
           Lwt.catch (fun () -> Lwt_unix.timeout delay)
                     (function
                      | Lwt_unix.Timeout ->
                       exec_handler t `RTIMEOUT;
                       t.last_read <- NetTimer.current_time ();
                       Lwt.return ()
                      | Lwt.Canceled ->
                         Lwt.return ()
                      | exn ->
                         Printf.eprintf "TcpClientSocket.timeout: Exception %s\n%!"
                                        (Printexc.to_string exn);
                         Lwt.return ()
                     )
           :: threads
         end
       else threads
     in
     if threads = [] then begin
         t.active_reader <- false;
         Lwt.return ()
       end else begin
         t.active_reader <- true;
         Lwt.bind
           (Lwt.pick threads)
           (fun () ->
             iter_read_actions t)
       end

let rec iter_write_actions t =
  match t.sock with
  | Closed _ -> Lwt.return ()
  | Socket fd ->
     let threads = [] in
     let threads =
       if TcpBuffer.length t.wbuf > 0 then
         Lwt.bind (TcpBuffer.write t.wbuf fd)
                  (fun nwrite ->
                    if nwrite = 0 then
                      close t Closed_by_peer
                    else begin
                        exec_handler t `CAN_REFILL;
                        if TcpBuffer.length t.wbuf = 0 then
                          exec_handler t `WRITE_DONE;
                      end;
                    Lwt.return ()
                  )
         :: threads
       else threads
     in
     if threads = [] then begin
         t.active_writer <- false;
         Lwt.return ()
       end else begin
         t.active_writer <- true;
         Lwt.bind
           (Lwt.pick threads)
           (fun () ->
             iter_write_actions t)
       end

let activate_reader t =
  if not t.active_reader then begin
      t.active_reader <- true;
      Lwt.async (fun () -> iter_read_actions t)
    end

let activate_writer t =
  if not t.active_writer then begin
      t.active_writer <- true;
      Lwt.async (fun () -> iter_write_actions t)
    end

(* val create : name:string -> Unix.file_descr -> handler -> t *)
let create ~name ?(max_buf_size = 1_000_000) fd handler =
  let sock = Socket fd in
  let rbuf = TcpBuffer.create max_buf_size in
  let wbuf = TcpBuffer.create max_buf_size in
  let active_reader = false in
  let active_writer = false in
  let rtimeout = -1. in
  let last_read = NetTimer.current_time () in
  let t = {
      name;
      sock;
      rbuf;
      wbuf;
      handler;
      active_reader;
      active_writer;
      last_read;
      rtimeout;
    } in
  activate_reader t;
  t


(* val connect: string -> Unix.file_descr -> int -> handler -> t *)
let connect _ = assert false


(* val write : t -> string -> pos:int -> len:int -> unit *)
let write t s ~pos ~len =
  if len < TcpBuffer.max_refill t.wbuf then begin
      TcpBuffer.add_bytes_from_string t.wbuf s pos len;
      activate_writer t
    end

let write_string t s =
  write t s ~pos:0 ~len:(String.length s)

(* val shutdown : t -> close_reason -> unit *)
let shutdown _ = assert false

(* val set_rtimeout : t -> float -> unit *)
let set_rtimeout t rtimeout =
  t.rtimeout <- rtimeout;
  activate_reader t

(* val set_wtimeout : t -> float -> unit *)
let set_wtimeout _ = assert false
(* val set_lifetime : t -> float -> unit *)
let set_lifetime _ = assert false


let string_of_event (event : tcpSocketEvent) =
  match event with
  | `CONNECTED -> "CONNECTED"
  | `READ_DONE n ->
     Printf.sprintf "READ_DONE %d" n
  | `CAN_REFILL -> "CAN_REFILL"
  | `WRITE_DONE -> "WRITE_DONE"
  | #NetTypes.event as event -> NetUtils.string_of_event event




let read_string t =
  let len = TcpBuffer.length t.rbuf in
  let s = Bytes.create len in
  TcpBuffer.read t.rbuf s 0 len;
  activate_reader t;
  s
let rlength t = TcpBuffer.length t.rbuf
let wlength t = TcpBuffer.length t.wbuf
let get t pos = TcpBuffer.get t.rbuf pos
let release_bytes t n =
  TcpBuffer.release_bytes t.rbuf n;
  activate_reader t
