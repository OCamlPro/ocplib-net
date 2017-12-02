(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v3.0            *)
(*   (GNU Lesser General Public Licence version 3.0).                     *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


open StringCompat

(* A simple client-server *)

let debug = false

type 'info connection = {
  info : 'info;
  fd : Lwt_unix.file_descr;
  mutable writer: unit Lwt.t;
}

let info con = con.info

let rec iter_write fd s pos len =
  if debug then Printf.eprintf "iter_write...\n%!";
  Lwt.bind (Lwt_unix.write fd s pos len)
    (fun nw ->
      if debug then Printf.eprintf "written %d\n%!" nw;
      if nw > 0 then
        let len = len - nw in
        if len > 0 then
          iter_write fd s (pos+nw) len
        else begin
          BytesCache.putback s;
          Lwt.return ()
        end
      else begin
        BytesCache.putback s;
        Lwt.return ()
      end
    )

let send_message con msg =
  if debug then Printf.eprintf "send_message...\n%!";
  let msg_len = String.length msg in
  let total_msg_len = 4+msg_len in
  let b = BytesCache.get total_msg_len in
  EndianString.LittleEndian.set_int32 b 0 (Int32.of_int msg_len);
  Bytes.blit msg 0 b 4 msg_len;
  con.writer <-
    (Lwt.bind con.writer (fun () ->
      iter_write con.fd b 0 total_msg_len));
  Lwt.async (fun () -> con.writer)

let shutdown con =
  Lwt_unix.shutdown con.fd Lwt_unix.SHUTDOWN_ALL

let close con =
  Lwt.async (fun () -> Lwt_unix.close con.fd)

module MakeSocket(S : sig

  type server_info
  type info

  val connection_info : server_info -> Unix.sockaddr -> info

  val connection_handler : info connection -> unit
  val message_handler : info connection -> string -> unit
  val disconnection_handler : info -> unit

end) = (struct

  let disconnection_handler con =
    try
      S.disconnection_handler con
    with exn ->
      Printf.eprintf "Warning: [disconnection_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let connection_handler con =
    try
      S.connection_handler con
    with exn ->
      Printf.eprintf "Warning: [connection_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let message_handler con msg =
    try
      S.message_handler con msg
    with exn ->
      Printf.eprintf "Warning: [message_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let rec iter_read con b pos =
    (* Printf.eprintf "\titer_read %d...\n%!" pos; *)
    let blen = Bytes.length b in
    let can_read = blen - pos in
    if can_read < 16384 then begin
      let newb = BytesCache.get (4 * blen) in
      Bytes.blit b 0 newb 0 pos;
      BytesCache.putback b;
      iter_read con newb pos;
    end
    else
      if Lwt_unix.state con.fd = Lwt_unix.Opened then
        Lwt.bind (Lwt_unix.read con.fd b pos can_read)
          (fun nr ->
             (* Printf.eprintf "\titer_read %d/%d...\n%!" nr pos; *)
             if nr > 0 then
               iter_parse con b nr pos
             else begin
               BytesCache.putback b;
               disconnection_handler con.info;
               Lwt.return ()
             end)
      else begin
        BytesCache.putback b;
        disconnection_handler con.info;
        Lwt.return ()
      end

  and iter_parse con b nr pos =
      (* Printf.eprintf "\titer_parse %d %d\n%!" nr pos; *)
    let pos = pos + nr in
    if pos > 4 then
      let msg_len = Int32.to_int
        (EndianString.LittleEndian.get_int32 b 0) in
        (* Printf.eprintf "\tmsg_len=%d\n" msg_len; *)
      let total_msg_len = msg_len + 4 in
      if total_msg_len > pos then
        iter_read con b pos
      else
        let msg = Bytes.sub b 4 msg_len in
        message_handler con msg;
        let nr = pos - total_msg_len in
        if nr > 0 then begin
          Bytes.blit b total_msg_len b 0 nr;
          iter_parse con b nr 0
        end else
          iter_read con b 0
    else
      iter_read con b pos

  let create ~loopback ?(port=0) context =
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let sockaddr = Unix.ADDR_INET(
      (if loopback then
          Unix.inet_addr_of_string "127.0.0.1"
       else
          Unix.inet_addr_any),
      port) in
    Lwt_unix.set_close_on_exec sock;
    Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
    Lwt_unix.bind sock sockaddr;
    Lwt_unix.listen sock 20;

    let rec iter_accept () =
      (* Printf.eprintf "\titer_accept...\n%!"; *)
      Lwt.bind (Lwt_unix.accept sock)
        (fun (fd, sock_addr) ->
          (* Printf.eprintf "\tServer received connection...\n%!"; *)
          let b = BytesCache.get (1 lsl 16) in
          let writer = Lwt.return () in
          let info = S.connection_info context sock_addr in
          let con = { info; fd; writer } in
          Lwt.async (fun () -> con.writer);
          connection_handler con;
          Lwt.async iter_accept;
          iter_read con b 0
        )

    in
    let port = match Unix.getsockname (Lwt_unix.unix_file_descr sock) with
        Unix.ADDR_INET(_, port) -> port
      | _ -> assert false in
    Lwt.async iter_accept;
    port

  let create_server = create

  let connect info sockaddr =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let writer = Lwt.return () in
    let con = { info; fd; writer } in
    let connected = ref true in
    con.writer <-
      (Lwt.bind
         (Lwt.catch
            (fun () -> Lwt_unix.connect fd sockaddr)
            (fun exn ->
               connected := false;
               disconnection_handler info;
               Lwt.return ()
            )
         )
         (fun () ->
            if !connected then begin
              if debug then Printf.eprintf "Connected\n%!";
              connection_handler con;
              let b = Bytes.create 65636 in
              Lwt.async (fun () -> iter_read con b 0);
            end ;
            Lwt.return ()
         ));
    con


end : sig

  val create : loopback:bool -> ?port:int -> S.server_info -> int
  val create_server : loopback:bool -> ?port:int -> S.server_info -> int
  val connect : S.info -> Lwt_unix.sockaddr -> S.info connection

end)


(* create sockets as server *)
module MakeServer(S : sig

  type server_info
  type info

  val connection_info : server_info -> Unix.sockaddr -> info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) = MakeSocket(S)

(* create sockets as client *)
module MakeClient(S : sig

  type info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) = MakeSocket(struct
  type server_info = S.info
  include S
  let connection_info _sockaddr = assert false
end
)
