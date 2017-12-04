open NetTypes

let debug = false

type event = tcpServerEvent

type 'info handler = 'info t -> event -> unit

 and 'info t = {
     mutable sock : socket;

     name : string;
     info : 'info;

     mutable sockaddr : Unix.sockaddr;
     mutable handler : 'info handler;
     mutable nconnections : int;
   }

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

let info t = t.info









let inet_addr_loopback = Unix.inet_addr_of_string "127.0.0.1"

let default_sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, 0)
let create ?(name="unknown")
           info
           sockaddr
           handler =
  let protocol = match sockaddr with
    | Unix.ADDR_INET _ -> Unix.PF_INET
    | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
  in
  let fd = Lwt_unix.socket protocol Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec fd;
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;

  let sock = Socket fd in
  let nconnections = 0 in
  let t = {
      sock;
      name;
      sockaddr;
      info;
      handler;
      nconnections;
    } in

  let rec iter_accept () =
    match t.sock with
    | Closed _ ->
       if debug then
         Printf.eprintf
           "Server socket closed. Not accepting connections anymore.\n%!";
       Lwt.return ()
    | Socket fd ->
       (* Printf.eprintf "\titer_accept...\n%!"; *)
       Lwt.bind (Lwt_unix.accept fd)
                (fun (fd, sock_addr) ->
                  if debug then
                    Printf.eprintf "\tServer received connection...\n%!";
                  Lwt.async (fun () ->
                      t.nconnections <- t.nconnections + 1;
                      exec_handler t (`CONNECTION (fd, sock_addr));
                      Lwt.return ()
                    );
                  iter_accept ()
                )
  in

  let bind_socket () =
    Lwt.bind
      (Lwt_unix.bind fd sockaddr)
      (fun () ->
        Lwt_unix.listen fd 20;
        t.sockaddr <- Unix.getsockname (Lwt_unix.unix_file_descr fd);
        exec_handler t `ACCEPTING;
        iter_accept ()
      )
  in
  Lwt.async bind_socket;
  t


let string_of_event (event : tcpServerEvent) =
  match event with
  | `CONNECTION (fd, sockaddr) ->
     Printf.sprintf "CONNECTION FROM %s"
                    (NetUtils.string_of_sockaddr sockaddr)
  | `ACCEPTING -> "ACCEPTING"
  | #NetTypes.event as event -> NetUtils.string_of_event event

(* val set_rtimeout : t -> float -> unit *)
let set_rtimeout _ = assert false
(* val set_wtimeout : t -> float -> unit *)
let set_wtimeout _ = assert false
(* val set_lifetime : t -> float -> unit *)
let set_lifetime _ = assert false

let nconnections t = t.nconnections
let sockaddr t = t.sockaddr
