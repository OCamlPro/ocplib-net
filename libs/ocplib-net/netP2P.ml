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

module StringMap = Map.Make(String)

type connection_state =
  CONNECTING
| CONNECTED
| IDENTIFIED
| DISCONNECTED


let string_of_state c =  match c with
    CONNECTING -> "CONNECTING"
  | CONNECTED -> "IDENTIFYING"
  | IDENTIFIED -> "IDENTIFIED"
  | DISCONNECTED -> "DISCONNECTED"


type peer_ip = string
type peer_id = string

type ('content, 'peer) message_type = {
    message_name : string;
    mutable message_writer : (Buffer.t -> 'content -> unit);
    mutable message_reader : (string -> int -> 'content);
    mutable message_printer : ('content -> string);
  }

let send_buf = Buffer.create 10000

type 'peer connection = {
    mutable conn_peer : 'peer option;
    mutable conn_sock : (unit TcpClientSocket.t) option;
    mutable conn_addr : peer_ip * int;
    mutable conn_state : connection_state;
  }

let peer_id_len = 16

let random_peer_id () = NetUtils.random_string peer_id_len
let string_of_ip ip = ip
let string_of_id id = Hex.encode id

module MakeNetwork ( M : sig

      type peer

      val peer_id : peer -> peer_id
      val new_peer : peer_id -> (peer_ip * int) option -> peer
      val peer_connection : peer -> peer connection option
      val set_peer_connection : peer -> peer connection option -> unit
      val peer_addr : peer -> (peer_ip * int) option
      val protocol_name : string

    end)
  = struct

    type peer = M.peer

    let peer_state p =
      match M.peer_connection p with
      | Some c -> c.conn_state
      | None -> DISCONNECTED


    let messages_by_name = ref StringMap.empty

    let declare_message message_name writer reader printer =
      if StringMap.mem message_name !messages_by_name then
        failwith (Printf.sprintf
            "declare_message: %s already defined in peer %s"
            message_name M.protocol_name);
      let m = {
          message_name = message_name;
          message_reader = reader;
          message_writer = writer;
          message_printer = printer;
        } in
      messages_by_name := StringMap.add message_name
        (Obj.magic m) !messages_by_name;
      m

    let parse_message msg pos =
      let message_name, pos = NetProto.String.get_string31 msg pos in
(*        let msg_num, pos = get_int msg pos in *)
      let m = try
          StringMap.find message_name !messages_by_name
        with Not_found ->
            Printf.eprintf "NetP2P.parse_message: unknown message %s in peer %s"
              message_name M.protocol_name;
            raise Not_found
      in
      Printf.eprintf "Received %s (Protocol %s)" message_name M.protocol_name;
      let msg =
        try
          m.message_reader msg pos
        with e ->
            Printf.eprintf "KernelClient.parse_message: exception %s\n%!"
              (Printexc.to_string e);
            raise e
      in
      Printf.eprintf "Parsed: %s\n%!" (m.message_printer  msg);
      Obj.magic (m,msg)

    let string_of_message m t = m.message_printer t

    module ConnectingMsg = struct

        type t = {
            id : peer_id;
(*            port : int; *)
          }

        let msg = declare_message "CONNECTING"
            (fun b body ->
              NetProto.buf_string8 b body.id;
          )
          (fun s pos ->
              let peer_id,pos = NetProto.String.get_string8 s pos in
(*              let port = try
                  NetProto.get_int16 s pos with _ -> 0 in *)
              {
                id = peer_id;
(*                port = port; *)
              })
          (fun body ->
              Printf.sprintf "CONNECTING {\n\tuid = %s\n  }\n%!"
                (Hex.encode body.id))
      end

    module ByeMsg = struct

        type t = {
            from_state : connection_state;
          }

        let msg = declare_message "BYE"
            (fun b body -> assert false )
          (fun s pos -> assert false )
          (fun body -> Printf.sprintf "BYE {\n\tfrom_state = %s\n } \n%!"
                (string_of_state body.from_state)
          )
      end

    module MakeUnitMsg (M : sig
          val name : string
        end ) =
      struct

        type t = unit

        let msg = declare_message M.name (fun b t -> ())
          (fun s pos -> ())
          (fun body -> Printf.sprintf "%s { } \n%!" M.name)
      end

    module IdentifiedMsg = struct

        type  t = {
            ip : peer_ip;
            port : int;
            my_port : int;
          }

        let msg = declare_message "IDENTIFIED"
            (fun b body ->
              NetProto.buf_string8 b body.ip;
              NetProto.buf_int16 b body.port;
              NetProto.buf_int16 b body.my_port;
          )
          (fun s pos ->
              let ip, pos = NetProto.String.get_string8 s pos in
              let port, pos =  NetProto.String.get_uint16 s pos in
              let my_port, _ =  try NetProto.String.get_uint16 s pos
                                with _ -> 0, pos in
              {
                ip =  ip;
                port = port;
                my_port = my_port;
              })
          (fun body ->
              Printf.sprintf "IDENTIFIED { \n\tip = %s\n\tport = %d\n\tmy_port = %d\n  }\n%!"
                (string_of_ip body.ip) body.port body.my_port)

      end

    let my_port = ref 0
    let set_my_port port = my_port := port
    let my_id = ref (random_peer_id ())
    let set_my_id id = my_id := id
    let get_my_id () = !my_id

    let message_handlers = ref StringMap.empty

    let peers_by_id = ref StringMap.empty
    let nmessages = ref 0

    let declare_handler m handler =
      message_handlers := StringMap.add m.message_name (Obj.magic
          (handler : peer connection -> 'a -> unit) )
      !message_handlers


    let write_message m t =

      Printf.eprintf "SENDING %s:\n%!" m.message_name;
      Printf.eprintf "%s\n%!" (m.message_printer t);

      NetProto.buf_string31 send_buf m.message_name;
      m.message_writer send_buf t


    let netstring_of_message m t =
      Buffer.clear send_buf;
      NetProto.buf_int31 send_buf 0;
      NetProto.buf_int31 send_buf !nmessages;
      incr nmessages;
      write_message m t;
      let b = Buffer.to_bytes send_buf in
      let len = Bytes.length b - 4 in
      NetProto.set_int31 b 0 len;
      Bytes.to_string b

    let do_handler c m msg =
      match
        try
          Some (StringMap.find m.message_name !message_handlers)
        with Not_found -> None
      with
        None ->
          Printf.eprintf "Warning: no handler for %s" m.message_name
      | Some f ->
          try
            f c msg
          with e ->
              Printf.eprintf "Exception %s in handler for %s\n%!"
                (Printexc.to_string e) m.message_name

(* Note: receiver_parse_message won't raise an exception unless something bad happened during parsing ! *)
    let receiver_parse_message c msg pos =
      let _message_counter = NetProto.String.get_int31 msg pos in
      let pos = pos + 4 in
      begin
        match c.conn_peer with
          None ->
            Printf.eprintf "No peer associated with connection"
        | Some p ->
            Printf.eprintf "New message from peer %s"
              (Hex.encode (M.peer_id p))
      end;
      let (m, msg) = parse_message msg pos in
      do_handler c m msg

    let read_cut_messages c t nread =
      let rec iter () =
        let buflen = TcpClientSocket.rlength t in
        if buflen >= 4 then
          let b = Bytes.create 4 in
          TcpClientSocket.blit t 0 b 0 4;
          let msg_len, pos = NetProto.Bytes.get_int31 b 0 in
          if buflen >= 4 + msg_len then begin
            TcpClientSocket.release t 4;
            let s = TcpClientSocket.read_string t msg_len in
            receiver_parse_message c s 0;
            iter ()
            end
      in
      iter ()

    let client_reader c b nread =
      read_cut_messages c b nread

    let rec parse_string c s pos len =
      if len >= 4 then
        let msg_len, new_pos = NetProto.String.get_int31 s pos in
        let total_length = 4 + msg_len in
        if len >= total_length then begin
            receiver_parse_message c s new_pos;
            parse_string c s (pos + total_length) (len - total_length)
          end else
          total_length - len
      else
        4 - len

    let last_sent_message () = !nmessages


    let sock_reader c sock nread =
      client_reader c sock nread

    let sock_closer c _ reason  =
      (match c.conn_sock with
          None -> ()
        | Some sock ->
            c.conn_sock <- None;
            TcpClientSocket.close sock reason);
      (match c.conn_peer with
          None -> ()
        | Some p ->
            (match M.peer_connection p with
                Some cc when cc == c ->
                  M.set_peer_connection p None;
                  do_handler c ByeMsg.msg {
                    ByeMsg.from_state = c.conn_state
                             };
                  peers_by_id := StringMap.remove
                                   (M.peer_id p) !peers_by_id
              | _ -> ());
            c.conn_peer <- None
      );
      c.conn_state  <- DISCONNECTED

    let sock_connected c sock sockaddr =
      c.conn_state <- CONNECTED


    let connection_send c msg content =
      match c.conn_sock with
        None -> failwith "NetP2P.send: peer disconnected"
      | Some sock ->
          begin
            match c.conn_peer with
              None -> ()
            | Some p ->
                Printf.eprintf "Sending message to peer %s"
                  (Hex.encode (M.peer_id p))
          end;
          TcpClientSocket.write_string_full sock
                                            (netstring_of_message msg content)

    let send_id c =
      connection_send c ConnectingMsg.msg {
        ConnectingMsg.id = !my_id ;
      }

    let init_connection sock conn_peer from_ip from_port  =
      let c = {
          conn_peer = conn_peer;
          conn_sock = Some sock;
          conn_addr = (from_ip, from_port);
          conn_state = CONNECTING;
        } in
      TcpClientSocket.set_reader sock (sock_reader c);
      TcpClientSocket.set_closer sock (sock_closer c);
      TcpClientSocket.set_rtimeout sock 30;
      (* TcpClientSocket.set_lifetime sock 120; *)
      TcpClientSocket.set_connected sock (sock_connected c);
      send_id c;
      c

    let bind_port port =
      let handler t event =
        match event with
        | `CONNECTION (fd, sockaddr) ->
           let from_ip, from_port =
             match sockaddr with
             | Unix.ADDR_INET(from_ip, from_port) ->
                NetUtils.string_of_inet_addr from_ip, from_port
             | _ -> assert false
           in
           Printf.eprintf "New Incoming Connecting from %s:%d"
                          (string_of_ip from_ip) from_port;

           let sock = TcpClientSocket.create
                        ~name:"client-helper connection" () fd in
           let _c = init_connection sock None from_ip from_port in
           ()
        | `ACCEPTING ->
           begin match TcpServerSocket.sockaddr t with
           | Unix.ADDR_INET(_,port) -> my_port := port
           | _ -> ()
           end
        | _ -> ()
      in
      let (__t : unit TcpServerSocket.t) =
        TcpServerSocket.create ~name:"server" ()
                               (Sockaddr.any port) ~handler
      in
      ()

    let disconnect p =
      match M.peer_connection p with
        None -> ()
      | Some c ->
          Printf.eprintf "NetP2P.disconnect called";
          sock_closer c () Closed_by_user

    let new_peer peer_id addr =
      try
        let p = StringMap.find peer_id !peers_by_id in
(*        (match p.peer_addr with
            None -> p.peer_addr <- addr
          | Some _ -> ()); *)
        p
      with Not_found ->
          let p = M.new_peer peer_id addr in
(*          {
            peer_id = peer_id;
            peer_addr = addr;
            peer_conn = None;
            } in *)
          peers_by_id := StringMap.add peer_id p !peers_by_id;
          (p : M.peer)

    let connect_addr ip port peer =
      let sock = TcpClientSocket.connect ()
                                         (Sockaddr.of_ip ip port) in
      let c = init_connection sock peer ip port in
      c

    let connect p =
      match M.peer_connection p with
      | Some conn ->
          Printf.eprintf "NetP2P.connect: peer already connected"
      | None ->
          match M.peer_addr p with
            None ->
              failwith "NetP2P.connect: peer address is unknown"
          | Some (ip,port) ->
              Printf.eprintf "Connecting to %s %s:%d"
                (Hex.encode (M.peer_id p)) (string_of_ip ip) port;
              let c = connect_addr ip port (Some p) in
              M.set_peer_connection p ( Some c )

    let set_rtimeout c n =
      match c.conn_sock with
        None -> ()
      | Some sock -> TcpClientSocket.set_rtimeout sock n

                                                  (*
    let set_lifetime c n =
      match c.conn_sock with
        None -> ()
      | Some sock -> TcpClientSocket.set_lifetime sock n
                                                   *)

    let set_max_output_buffer c n =
      match c.conn_sock with
        None -> ()
      | Some sock -> TcpClientSocket.set_max_output_buffer sock n


    let connection_identified c =
      c.conn_state <- IDENTIFIED;
      set_rtimeout c 7200;
      let (ip, port) = c.conn_addr in
      connection_send c IdentifiedMsg.msg {
        IdentifiedMsg.ip = ip;
        port = port;
        my_port = !my_port;
      }

    let _ =
      declare_handler ConnectingMsg.msg (fun
          (c :  peer connection) m ->
          let (p : M.peer) = new_peer m.ConnectingMsg.id (Some c.conn_addr) in
          match c.conn_peer with
            Some (pp : M.peer) when p != pp ->
              Printf.eprintf "connectingMsg: unexpected peer"
          | Some p ->
              connection_identified c
          | None ->
              match M.peer_connection p with
                None ->
                  c.conn_peer <- Some p;
                  M.set_peer_connection p ( Some c );
                  connection_identified c
              | Some cc when cc == c -> assert false
              | Some cc ->
                  match cc.conn_state with
                    IDENTIFIED ->
                      Printf.eprintf "Connecting to already IDENTIFIED peer: disconnecting";
                      sock_closer c () Closed_by_user
                  | _ ->
                      Printf.eprintf "Connecting to already connected peer: disconnecting";
                      sock_closer cc () Closed_by_user;
                      c.conn_peer <- Some p;
                      M.set_peer_connection p  ( Some c );
                      connection_identified c
      )

    let connect_addr ip port =
      connect_addr ip port None

    let declare_message m =
      if m = "CONNECTING" then
        failwith "Cannot redefine message CONNECTING";
      declare_message m

    let connect_addr ip port =
      let _c = connect_addr ip port in
      ()

    let declare_handler m handler =
      declare_handler m (fun c msg ->
          if c.conn_state = IDENTIFIED ||  m.message_name = "BYE" then
            match c.conn_peer with
            | Some p ->
                handler p msg
            | None ->
                Printf.eprintf "FOR message %s" m.message_name;
                Printf.eprintf "No peer attached for message: disconnecting";
                sock_closer c () (Closed_for_error "no peer attached")
          else begin
                Printf.eprintf "FOR message %s" m.message_name;
                Printf.eprintf "Peer not identified for message: disconnecting";
                sock_closer c ()  (Closed_for_error "not identified")
            end
      )

    let send p msg content =
      match M.peer_connection p with
        None ->
          Printf.eprintf "While sending %s:\n%!" msg.message_name;
          Printf.eprintf "%s\n%!" (msg.message_printer content);

          failwith "NetP2P.send: peer not connected"
      | Some c ->
          if c.conn_state = IDENTIFIED then
            connection_send c msg content

    let get_peer id = StringMap.find id !peers_by_id
    let iter_peers f =
      StringMap.iter (fun _ p -> f p) !peers_by_id

    let set_rtimeout p n =
      match M.peer_connection p with
        None -> ()
      | Some c -> set_rtimeout c n

    let set_max_output_buffer p n =
      match M.peer_connection p with
        None -> ()
      | Some c -> set_max_output_buffer c n

  end
