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

let should_close = ref false

module Socket = Concur.MakeSocket(struct

    type info = string list ref
    type server_info = string

    let disconnection_handler con =
      Printf.eprintf "Disconnected\n%!";
      if !should_close then Concur.exit ()

    let message_handler con msg =
      Printf.eprintf "Received %S from connection\n%!" msg;
      let info = Concur.info con in
      match !info with
        [] ->
        if !should_close then begin
          Concur.shutdown con;
          (*          Concur.close con *)
        end else
          Concur.send_message con "Message received"
      | s :: tail ->
        info := tail;
        Concur.send_message con s

    let connection_handler con =
      Printf.eprintf "New Connnection !\n%!";
      let info = Concur.info con in
      match !info with
        [] -> Concur.shutdown con; Concur.close con
      | s :: tail ->
         info := tail;
         Printf.eprintf "Sending message\n%!";
        Concur.send_message con s

    let connection_info sv sock_addr = ref [sv]

    let accepting_handler sv sock_addr =
      match sock_addr with
      | Unix.ADDR_INET(_,port) ->
         Printf.eprintf "Started on port %d\n%!" port
      | _ -> assert false

  end)


let () =
  begin
    if Array.length Sys.argv = 1 then begin
        let () = Socket.create_server  ~loopback:true "I am the server" in
        Printf.eprintf "Server initialized\n%!";
      ()
    end else
      let addr = Unix.inet_addr_of_string "127.0.0.1" in
      let port = int_of_string Sys.argv.(1) in
      let sockaddr = Unix.ADDR_INET (addr, port) in
      let _con = Socket.connect  (ref [ "Hello server"; "How are you ?" ]) sockaddr in
      should_close := true
  end;
  Concur.main ()
