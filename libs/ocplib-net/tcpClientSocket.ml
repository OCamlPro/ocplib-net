
open NetTypes

(* BufferOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow = TcpBuffer.BufferWriteOverflow
exception BufferReadOverflow = TcpBuffer.BufferReadOverflow

type event = tcpSocketEvent

type 'info t = {
    mutable sock : socket;

    name : string;
    info : 'info;

    mutable connecting : Unix.sockaddr option;
    mutable connected : bool;

    mutable active_reader : bool;
    mutable active_writer : bool;
    mutable handler : 'info handler;
    mutable wbuf : TcpBuffer.t;
    mutable rbuf : TcpBuffer.t;

    mutable nread : int;
    mutable last_read : float;
    mutable rtimeout : float;

    mutable nwritten : int;
    mutable last_write : float;
    mutable wtimeout : float;
  }

and 'info handler = 'info t -> event -> unit



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





let rec iter_write_actions t =
  match t.sock with
  | Closed _ -> Lwt.return ()
  | Socket fd ->
     t.active_writer <- true;
     let threads = [] in
     let threads =
       match t.connecting with
       | Some _sockaddr -> threads
       | None ->
          if TcpBuffer.length t.wbuf > 0 then
            Lwt.bind (TcpBuffer.write t.wbuf fd)
                     (fun nwrite ->
                       if nwrite = 0 then
                         close t Closed_by_peer
                       else begin
                           t.nwritten <- t.nwritten + nwrite;
                           exec_handler t `CAN_REFILL;
                           if TcpBuffer.length t.wbuf = 0 then
                             exec_handler t `WRITE_DONE;
                         end;
                       Lwt.return ()
                     )
            :: threads
          else threads
     in
     let threads =
       if threads != [] && t.wtimeout > 0. then begin
           let delay = t.wtimeout -.
                         (NetTimer.current_time () -. t.last_write)
           in
           (* Printf.eprintf "delay: %.1f\n%!" delay; *)
           let delay = if delay < 0.01 then 0.01 else delay in
           Lwt.catch (fun () -> Lwt_unix.timeout delay)
                     (function
                      | Lwt_unix.Timeout ->
                         exec_handler t `WTIMEOUT;
                         t.last_write <- NetTimer.current_time ();
                         Lwt.return ()
                      | Lwt.Canceled ->
                         Lwt.return ()
                      | exn ->
                         Printf.eprintf
                           "TcpClientSocket.timeout: Exception %s\n%!"
                           (Printexc.to_string exn);
                         Lwt.return ()
                     )
           :: threads
         end
       else threads
     in
     if threads = [] then begin
         t.active_writer <- false;
         (* Printf.eprintf "Nothing to write...\n%!"; *)
         Lwt.return ()
       end else begin
         Lwt.bind
           (Lwt.pick threads)
           (fun () ->
             iter_write_actions t)
       end

let activate_writer t =
  (* Printf.eprintf "activate_writer...\n%!";  *)
  if not t.active_writer then begin
      t.active_writer <- true;
      Lwt.async (fun () -> iter_write_actions t)
    end

let rec iter_read_actions t =
  match t.sock with
  | Closed _ -> Lwt.return ()
  | Socket fd ->
     t.active_reader <- true;
     let threads = [] in
     let threads =
       match t.connecting with
       | Some sockaddr ->
          Lwt.catch
            (fun () ->
              Lwt.bind (Lwt_unix.connect fd sockaddr)
                       (fun () ->
                         t.connecting <- None;
                         t.last_read <- NetTimer.current_time ();
                         t.last_write <- NetTimer.current_time ();
                         t.connected <- true;
                         exec_handler t `CONNECTED;
                         activate_writer t;
                         Lwt.return ()
                       ))
            (fun exn ->
              t.connecting <- None;
              close t Closed_by_peer;
              Lwt.return ()
            )
          :: threads
       | None ->
          if not t.connected then begin
              t.connected <- true;
              exec_handler t `CONNECTED;
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
                else begin
                    t.last_read <- NetTimer.current_time ();
                    (* Printf.eprintf "last_read %.0f\n%!" t.last_read; *)
                    t.nread <- t.nread + nread;
                    exec_handler t (`READ_DONE nread);
                  end;
                Lwt.return ()
              )
            :: threads
          else threads
     in
     let threads =
       if threads != [] && t.rtimeout > 0. then begin
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
         Lwt.bind
           (Lwt.pick threads)
           (fun () ->
             iter_read_actions t)
       end

let activate_reader t =
  if not t.active_reader then begin
      t.active_reader <- true;
      Lwt.async (fun () -> iter_read_actions t)
    end

(* val create : name:string -> Unix.file_descr -> handler -> t *)
let create ?(name="unknown") ?(max_buf_size = 1_000_000)
           ?connecting
           info
           fd
           handler =
  let sock = Socket fd in
  let rbuf = TcpBuffer.create max_buf_size in
  let wbuf = TcpBuffer.create max_buf_size in
  let active_reader = false in
  let active_writer = false in
  let rtimeout = -1. in
  let wtimeout = -1. in
  let last_read = NetTimer.current_time () in
  let last_write = NetTimer.current_time () in
  let nread = 0 in
  let nwritten = 0 in
  let connected = false in
  let t = {
      name;
      sock;
      rbuf;
      wbuf;
      info;
      handler;
      connecting;
      connected;
      active_reader;
      active_writer;
      last_read;
      rtimeout;
      last_write;
      wtimeout;
      nread;
      nwritten;
    } in
  activate_reader t;
  t


(* val connect: string -> Unix.file_descr -> int -> handler -> t *)
let connect ?name ?max_buf_size info sockaddr handler =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  create ?name ?max_buf_size ?connecting:(Some sockaddr) info fd handler

let create ?name ?max_buf_size info fd handler =
  create ?name ?max_buf_size info fd handler

(* val write : t -> string -> pos:int -> len:int -> unit *)
let write t s ~pos ~len =
  TcpBuffer.add_bytes_from_string t.wbuf s pos len;
  activate_writer t

let write_string t s =
  write t s ~pos:0 ~len:(String.length s)

(* val shutdown : t -> close_reason -> unit *)
let shutdown t reason =
  match t.sock with
  | Closed _ -> ()
  | Socket fd ->
     Lwt_unix.shutdown fd Lwt_unix.SHUTDOWN_ALL;
     close t reason

(* val set_rtimeout : t -> float -> unit *)
let set_rtimeout t rtimeout =
  t.rtimeout <- rtimeout;
  activate_reader t

(* val set_wtimeout : t -> float -> unit *)
let set_wtimeout t wtimeout =
  t.wtimeout <- wtimeout;
  activate_writer t

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




let blit t pos0 s pos len =
  TcpBuffer.blit t.rbuf pos0 s pos len
let release t n =
  TcpBuffer.release_bytes t.rbuf n
let read t s pos len =
  TcpBuffer.read t.rbuf s pos len;
  activate_reader t
let read_string t =
  let len = TcpBuffer.length t.rbuf in
  let s = Bytes.create len in
  read t s 0 len;
  s
let rlength t = TcpBuffer.length t.rbuf
let wlength t = TcpBuffer.length t.wbuf
let get t pos = TcpBuffer.get t.rbuf pos
let release_bytes t n =
  TcpBuffer.release_bytes t.rbuf n;
  activate_reader t

let nwritten t = t.nwritten
let nread t = t.nread
let info t = t.info
