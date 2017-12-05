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



(* SOCKETS *)


type 'info connection

(* [Concur.info con] extracts the [info] associated with connection
   [con]. *)
val info : 'info connection -> 'info

(* [Concur.send_message con msg] sends a message [msg] on connection
   [con], that is supposed to be already connected. Note that the
   message is not sent directly on the socket, but embedded in special
   format that will help the receiver extracts the message again. *)
val send_message : 'info connection -> string -> unit

(* [Concur.shutdown con] shutdowns a socket, i.e. flushes it as much
   as possible before closing it. *)
val shutdown : 'a connection -> unit

(* [Concur.close con] closes a socket. *)
val close : 'a connection -> unit

(* create sockets either as server or as client *)
module MakeSocket(
    S : sig

      (* [server_info] is the type of the user state of a server. *)
      type server_info

      (* [info] is the type of the user state of a connection. It is
         often useful to put in this state whether the connection is
         connected or not. *)
      type info


      val accepting_handler : server_info -> Unix.sockaddr -> unit


      (* When a connection is received on a server,
         [connection_info server_info sockaddr] is called to
         initialize the user state of this new connection. *)
      val connection_info : server_info -> Unix.sockaddr -> info

      (* [connection_handler con] is called when a new connection is
         established, i.e. either a new connection to a server, or a
         client [connect] has succeeded. *)
      val connection_handler : info connection -> unit

      (* [message_handler con msg_content] is called to handle a new
         message received on an established connection. *)
      val message_handler : info connection -> string -> unit

      (* [disconnection_handler info] is called when a socket is
         either disconnected, or when a client [connect] has failed.
         For [connect], it is the task of the user to discriminate
         between a failed connect, and a connection that is closed. *)
      val disconnection_handler : info -> unit

end) : sig

  (* [create_server ~loopback ?port server_info] creates a server on
     port [port] (possibly 0), on either the loopback address or any
     interface ([loopback]), with initial state [server_info]. *)
  val create_server : loopback:bool -> ?port:int -> S.server_info -> unit
  val create : loopback:bool -> ?port:int -> S.server_info -> unit

  (* [connect info sockaddr] connects to the given [sockaddr]
     (Unix.ADDR_INET(addr,port), and initializes the connection state
     with [info]. *)
  val connect : S.info -> Unix.sockaddr -> S.info connection

end

(* [MakeServer(...)] is a simplified version of [MakeSocket] for
   a server-only application. *)
module MakeServer(S : sig
  type server_info
  type info

  val accepting_handler : server_info -> Unix.sockaddr -> unit

  val connection_info : server_info -> Unix.sockaddr -> info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) : sig

  (* Same as [MakeSocket(...).create_server *)
  val create : loopback:bool -> ?port:int -> S.server_info -> unit

end

(* [MakeClient(...)] is a simplified version of [MakeSocket] for
   a client-only application. *)
module MakeClient(S : sig

  type info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) : sig

  (* [connect conn_id sockaddr] connects to the given sockaddr.
     handlers are called with the conn_id. *)
  val connect : S.info -> Unix.sockaddr -> S.info connection

end






(* PROCESSES *)

(* [exec exe_name args exit_handler], filenames can be specified to be
   read for stdin, and written for stdout/stderr. Can raise an error if
   stdin file does not exist/not readable, or stdout/stderr files cannot
   be created/written to. *)
val exec : string -> string array -> ?timeout:float ->
  ?stdin:string -> ?stdout:string -> ?stderr:string ->
  (Unix.process_status -> unit) -> unit




(* MAIN LOOP *)




(* force exit from Lwt loop (can be delayed by 0.1s) *)
val exit : unit -> unit

(* loop in Lwt until [exit] is called *)
val main : unit -> unit




(* TIMERS *)



module Timer : sig

  type t
  (* [create n f] calls [f] after [n] seconds. *)
  val create : float -> (unit -> unit) -> t
  (* [stop t] prevents [t] from happening *)
  val stop : t -> unit

end
