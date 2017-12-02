open NetTypes

let debug = false

type event = tcpServerEvent

type handler = t -> event -> unit

 and t = {
     mutable sock : socket;

     name : string;
     addr : Unix.inet_addr;

     mutable port : int;
     mutable handler : handler;
   }

let exec_handler t event =
  try
    t.handler t event
  with exn ->
    Printf.eprintf "TcpServerSocket: exception %S in handler\n%!"
                   (Printexc.to_string exn)

let inet_addr_loopback = Unix.inet_addr_of_string "127.0.0.1"

(* val create : name:string -> Unix.inet_addr -> int -> handler -> t *)
let create ~name ?(addr=Unix.inet_addr_any) ?(port=0) handler =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET(addr, port) in
  Lwt_unix.set_close_on_exec fd;
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;

  let sock = Socket fd in
  let t = {
      sock;
      name;
      addr;
      port;
      handler;
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
        let port = match Unix.getsockname (Lwt_unix.unix_file_descr fd) with
            Unix.ADDR_INET(_, port) -> port
          | _ -> assert false in
        t.port <- port;
        exec_handler t (`ACCEPTING port);
        iter_accept ()
      )
  in
  Lwt.async bind_socket;
  t

let port t = t.port

(* val close : t -> close_reason -> unit *)

(* val set_rtimeout : t -> float -> unit *)
(* val set_lifetime : t -> float -> unit *)
(* val set_handler : t -> handler -> unit *)

(* val handler : t -> handler *)
(* val closed : t -> bool *)

let string_of_event (event : tcpServerEvent) =
  match event with
  | `CONNECTION (fd, addr) ->
     Printf.sprintf "CONNECTION FROM %s"
                    (NetUtils.string_of_sockaddr addr)
  | `ACCEPTING port ->
     Printf.sprintf "ACCEPTING %d" port
  | #NetTypes.event as event -> NetUtils.string_of_event event

let close t reason =
  match t.sock with
  | Closed _ -> ()
  | Socket fd ->
     t.sock <- Closed reason;
     Lwt_unix.close fd;
     exec_handler t (`CLOSED reason)

let closed t =
  match t.sock with
  | Socket _ -> false
  | Closed _ -> true

let handler t = t.handler
let set_handler t h = t.handler <- h

let set_lifetime t lifetime = assert false (* TODO *)
let set_rtimeout t rtimeout = assert false (* TODO *)
let set_wtimeout t wtimeout = assert false (* TODO *)
