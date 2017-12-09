
open NetTypes

exception PacketWriteOverflow of int * int * int

type event = udpSocketEvent

type 'info t = {
    mutable sock : NetTypes.socket;
    mutable info : 'info;

    mutable connected : bool;

    buf : bytes;
    mutable rbuf : message Queue.t;
    mutable rsize : int;
    mutable max_rsize : int;

    mutable rtimeout : float;
    mutable last_read : float;

    mutable wbuf : message Queue.t;
    mutable wsize : int;
    mutable max_wsize : int;

    mutable sockaddr : Unix.sockaddr;
    mutable handler : 'info handler;

    events : event Queue.t;
    mutable wakener : unit Lwt.u option;
  }

 and 'info handler = 'info t -> udpSocketEvent -> unit


let string_of_event (t : udpSocketEvent) =
  match t with
  | `ACCEPTING -> "ACCEPTING"
  | `READ_DONE -> "READ_DONE"
  | `CAN_REFILL -> "CAN_REFILL"
  | `WRITE_DONE -> "WRITE_DONE"
  | #NetTypes.event as event -> NetUtils.string_of_event event


let exec_handler t event =
  try
    t.handler t event
  with exn ->
    Printf.eprintf "UdpSocket: exception %S in handler\n%!"
                   (Printexc.to_string exn)



let activate_thread t =
  match t.wakener with
  | None -> () (* Not useful, the thread is already awake *)
  | Some u ->
     Printf.eprintf "wakeup\n%!";
     Lwt.wakeup u ();
     t.wakener <- None

(* val close : t -> close_reason -> unit *)
let close t reason =
  match t.sock with
  | Closed _ -> ()
  | Closing (_fd, _reason) -> ()
  | Socket fd ->
     t.sock <- Closing (fd, reason);
     activate_thread t

let queue_event t event =
  Queue.add event t.events

let exec_events t =
  while not (Queue.is_empty t.events) do
    let event = Queue.take t.events in
    exec_handler t event;
    match event with
    | `CAN_REFILL ->
       if t.wsize = 0 then
         exec_handler t `WRITE_DONE;
    | _ -> ()
  done

(* val closed : t -> bool *)
let closed t =
  match t.sock with
  | Socket _ -> false
  | Closing _
  | Closed _ -> true

let handler t = t.handler
let set_handler t handler = t.handler <- handler
let info t = t.info
let sockaddr t = t.sockaddr

let rlength t = Queue.length t.rbuf
let wlength t = Queue.length t.wbuf

let rec iter_socket t =
  (* Printf.eprintf "iter_actions\n%!";*)
  t.wakener <- None;
  match t.sock with
  | Closing (fd, reason) ->
     t.sock <- Closed reason;
     exec_handler t (`CLOSED reason);
     Lwt.catch (fun () ->
         Lwt.bind (Lwt_unix.close fd)
                  (fun () ->
                    Lwt.return ()
       ))
               (fun exn ->
                 Lwt.return ()
               )

  | Closed _ -> Lwt.return ()
  | Socket fd ->

     let read_threads = [] in

     let read_threads =
       if t.rsize < t.max_rsize then
         Lwt.bind (Lwt_unix.recvfrom fd t.buf 0 65536 [])
                  (fun (nread, msg_sockaddr) ->
                    if nread > 0 then begin
                        let msg_content = String.sub t.buf 0 nread in
                        let u = { msg_content;
                                  msg_sockaddr } in
                        Queue.add u t.rbuf;
                        t.last_read <- NetTimer.current_time ();
                        t.rsize <- t.rsize + nread;
                        queue_event t `READ_DONE;
                      end;
                    Lwt.return ()
                  ) :: read_threads
       else
         read_threads
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
                         Lwt.return ()
                      | Lwt.Canceled ->
                         Lwt.return ()
                      | exn ->
                         Printf.eprintf "TcpClientSocket.timeout: Exception %s\n%!"
                                        (Printexc.to_string exn);
                         Lwt.return ()
                     )
           :: read_threads
         end
       else read_threads
     in


     let write_threads = [] in

     let write_threads =
       if t.wsize > 0 then
         let u = Queue.peek t.wbuf in
         (* Printf.eprintf "Preparing to write\n%!"; *)
         Lwt.bind (Lwt_unix.sendto fd u.msg_content 0
                                   (String.length u.msg_content)
                                   [] u.msg_sockaddr)
                  (fun nwritten ->
                    if nwritten > 0 then begin
                        let u = Queue.take t.wbuf in
                        t.wsize <- t.wsize - String.length u.msg_content;
                        queue_event t `CAN_REFILL;
                      end;
                    Lwt.return ()
                  ) :: write_threads
       else
         write_threads
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

let create info sockaddr handler =

  let domain = match sockaddr with
    | Unix.ADDR_INET _ -> Unix.PF_INET
    | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
  in
  let fd = Lwt_unix.socket domain Unix.SOCK_DGRAM 0 in
  Lwt_unix.set_close_on_exec fd;
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;

  let buf = Bytes.create 65536 in
  let sock = Socket fd in
  let connected = false in
  let rtimeout = -1. in
  let last_read = NetTimer.current_time () in
  let rbuf = Queue.create () in
  let wbuf = Queue.create () in
  let rsize = 0 in
  let wsize = 0 in
  let max_rsize = 1_000_000 in
  let max_wsize = 1_000_000 in
  let events = Queue.create () in
  let wakener = None in

  let t = {
      sock;
      connected;

      buf;
      rbuf;
      rsize;
      max_rsize;

      wbuf;
      wsize;
      max_wsize;

      rtimeout;
      last_read;

      info;
      sockaddr;
      handler;

      events;
      wakener;
    } in
  (* TODO: we should probably move this in the iter_socket automata *)
  let bind_socket () =
    Lwt.catch (fun () ->
        Lwt.bind
          (Lwt_unix.bind fd sockaddr)
          (fun () ->
            t.sockaddr <- Unix.getsockname (Lwt_unix.unix_file_descr fd);
            exec_handler t `ACCEPTING;
            t.last_read <- NetTimer.current_time ();
            iter_socket t
      ))
              (fun exn ->
                Printf.eprintf "UdpSocket: exception in bind %s\n%!"
                               (Printexc.to_string exn);
                Lwt.return () (* TODO: what shall we do *)
              )
  in
  Lwt.async bind_socket;
  t

let read t =
  let u = Queue.take t.rbuf in
  t.rsize <- t.rsize - String.length u.msg_content;
  u

let rec read_packets t h =
  match read t with
  | exception Queue.Empty ->
     activate_thread t
  | u ->
     (try h u with exn ->
        Printf.eprintf "UdpSocket: exception %S in message handler\n%!"
                   (Printexc.to_string exn)
     );
     read_packets t h

let write t msg_content msg_sockaddr =
  let len = String.length msg_content in
  let new_wsize = t.wsize + len in
  if new_wsize > t.max_wsize then
    raise (PacketWriteOverflow (t.wsize, len, t.max_wsize));
  let u = { msg_content; msg_sockaddr } in
  Queue.add u t.wbuf;
  t.wsize <- new_wsize;
  activate_thread t
