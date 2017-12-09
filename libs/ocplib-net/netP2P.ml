(*

(*
  Let's give a template of code:



type peer_info = {
    mutable peer_conn : peer_info connection option;
    mutable peer_id : Sha1.t;
    mutable peer_addr : (Ip.t * int) option;
(*    ... complete with whatever you need *)
  }


module PaceMakerNetwork = NetP2P.MakeNetwork (struct
      type peer = peer_info

      let new_peer peer_id peer_addr =
        {
          peer_id = peer_id;
          peer_addr = peer_addr;
          peer_conn = None;
        }
      let peer_id p = p.peer_id
      let peer_address p = p.peer_addr
      let peer_connection p = p.peer_conn
      let set_peer_connection p c = p.peer_conn <- c

      let protocol_name = "pace-maker"
    end)

let _ =
  PaceMakerNetwork.declare_handler PaceMakerNetwork.IdentifiedMsg.msg
    (fun p msg ->
(* This is run when a peer as just been identified *)
      Printf.printf "Connected to %s\n%!"
        (Sha1.to_string p.peer_id)
  );
  PaceMakerNetwork.declare_handler PaceMakerNetwork.ByeMsg.msg
    (fun p msg ->
      Printf.printf "Disconnecting from %s (%s)\n%!"
        (Sha1.to_string p.peer_id) (string_of_state msg.PaceMakerNetwork.ByeMsg.from_state)
  );
  PaceMakerNetwork.set_my_id  !uid;
  let _s = PaceMakerNetwork.bind_port !port in
  ()

*)


module I : sig

    type connection_state =
      CONNECTING
    | CONNECTED
    | IDENTIFIED
    | DISCONNECTED

    val string_of_state : connection_state -> string

    type ('a, 'b) message_type = {
        message_name : string;
        mutable message_writer : Buffer.t -> 'a -> unit;
        mutable message_reader : string -> int -> 'a;
        mutable message_printer : 'a -> string;
      }

    type 'a connection = {
        mutable conn_peer : 'a option;
        mutable conn_sock : TcpSocket.t option;
        mutable conn_addr : Ip.t * int;
        mutable conn_state : connection_state;
      }

    module MakeNetwork :
      functor
      (M : sig
          type peer
          val peer_id : peer -> Checksum.Sha1.t
          val new_peer : Checksum.Sha1.t -> (Ip.t * int) option -> peer
          val peer_connection : peer -> peer connection option
          val set_peer_connection :
            peer -> peer connection option -> unit
          val peer_address : peer -> (Ip.t * int) option
          val protocol_name : string
        end) ->
      sig
        type peer = M.peer
        val declare_message :
          string ->
          (Buffer.t -> 'a -> unit) ->
          (string -> int -> 'a) ->
          ('a -> string) -> ('a, peer) message_type
        val declare_handler :
          ('a, peer) message_type -> (peer -> 'a -> unit) -> unit
        val send : peer -> ('a, peer) message_type -> 'a -> unit
        val new_peer : Checksum.Sha1.t -> (Ip.t * int) option -> peer
        val connect : peer -> unit
        val connect_addr : Ip.t -> int -> unit
        val disconnect : peer -> unit
        val bind_port : int -> TcpServerSocket.t
        val set_my_port : int -> unit
        val set_my_id : Checksum.Sha1.t -> unit
        val get_my_id : unit -> Checksum.Sha1.t
        val peer_state : peer -> connection_state

        val set_rtimeout : peer -> int -> unit
        val set_lifetime : peer -> int -> unit
        val set_max_output_buffer : peer -> int -> unit

          module IdentifiedMsg :
            sig
              type t = { ip : Ip.t; port : int; my_port : int }
              val msg : (t, 'a) message_type
            end
          module ByeMsg :
            sig
              type t = { from_state : connection_state; }
              val msg : (t, 'a) message_type
          end


        module MakeUnitMsg : functor (M : sig val name : string end) ->
          sig type t = unit val msg : (unit, 'a) message_type end

        val iter_peers : (peer -> unit) -> unit
        val get_peer : Checksum.Sha1.t -> peer

      end

  end = struct



open   NetBuf
open   BasicSocket
open   Sets
open Checksum
open ConnectionManager


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


type ('content, 'peer) message_type = {
    message_name : string;
    mutable message_writer : (Buffer.t -> 'content -> unit);
    mutable message_reader : (string -> int -> 'content);
    mutable message_printer : ('content -> string);
  }

let send_buf = Buffer.create 10000

type 'peer connection = {
    mutable conn_peer : 'peer option;
    mutable conn_sock : TcpSocket.t option;
    mutable conn_addr : Ip.t * int;
    mutable conn_state : connection_state;
  }

module MakeNetwork ( M : sig

      type peer

      val peer_id : peer -> Sha1.t
      val new_peer : Sha1.t -> (Ip.t * int) option -> peer
      val peer_connection : peer -> peer connection option
      val set_peer_connection : peer -> peer connection option -> unit
      val peer_address : peer -> (Ip.t * int) option
      val protocol_name : string



    end)
(*
  : sig
    type peer = M.peer

    val declare_message : string ->
      (Buffer.t -> 'a -> unit) ->
      (string -> int -> 'a) ->
      ('a -> string) ->
      ('a, peer) message_type

    val declare_handler :
      ('a, peer) message_type -> ( peer -> 'a -> unit) -> unit
    val send :  peer -> ('a, peer) message_type -> 'a -> unit
    val new_peer : Checksum.Sha1.t -> (Ip.t * int) option ->  peer
    val connect : peer -> unit
    val connect_addr : Ip.t -> int -> unit
    val disconnect :  peer -> unit
    val bind_port : int -> TcpServerSocket.t
    val set_my_id : Checksum.Sha1.t -> unit
    val get_my_id : unit -> Checksum.Sha1.t

    val peer_state : peer -> connection_state

    module IdentifiedMsg : sig
        type t = { ip : Ip.t; port :  int }

        val msg : (t, 'a) message_type
      end

    module ByeMsg : sig
        type t = { from_state : connection_state }

        val msg : (t, 'a) message_type
      end

end
*)
  = struct

    type peer = M.peer

(*
  {
        mutable peer_addr : (Ip.t * int) option;
        mutable peer_id : Sha1.t;
        mutable peer_conn : 'peer connection option;
      }
*)


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
      let message_name, pos = LittleEndian.get_string31 msg pos in
(*        let msg_num, pos = get_int msg pos in *)
      let m = try
          StringMap.find message_name !messages_by_name
        with Not_found ->
            if not (debug level_D flag_P) then () else lprintf "NetP2P.parse_message: unknown message %s in peer %s"
              message_name M.protocol_name;
            raise Not_found
      in
      if not (debug level_DDD flag_P) then () else lprintf "Received %s (Protocol %s)" message_name M.protocol_name;
      let msg =
        try
          m.message_reader msg pos
        with e ->
            if not (debug level_D flag_P) then () else lprintf "KernelClient.parse_message: exception %s\n"
              (Printexc2.to_string e);
            raise e
      in
      if not (debug level_DDD flag_P) then () else lprintf "Parsed: %s\n" (m.message_printer  msg);
      Obj.magic (m,msg)

    let string_of_message m t = m.message_printer t

    module ConnectingMsg = struct

        type t = {
            id : Sha1.t;
(*            port : int; *)
          }

        let msg = declare_message "CONNECTING"
            (fun b body ->
              NetProto.buf_sha1 b body.id;
          )
          (fun s pos ->
              let peer_id,pos = NetProto.get_sha1 s pos in
(*              let port = try
                  LittleEndian.get_int16 s pos with _ -> 0 in *)
              {
                id = peer_id;
(*                port = port; *)
              })
          (fun body ->
              Printf.sprintf "CONNECTING {\n\tuid = %s\n  }\n"
                (Sha1.to_string body.id))
      end

    module ByeMsg = struct

        type t = {
            from_state : connection_state;
          }

        let msg = declare_message "BYE"
            (fun b body -> assert false )
          (fun s pos -> assert false )
          (fun body -> Printf.sprintf "BYE {\n\tfrom_state = %s\n } \n"
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
          (fun body -> Printf.sprintf "%s { } \n" M.name)
      end

    module IdentifiedMsg = struct

        type  t = {
            ip : Ip.t;
            port : int;
            my_port : int;
          }

        let msg = declare_message "IDENTIFIED"
            (fun b body ->
              NetProto.buf_ip b body.ip;
              NetProto.buf_int b body.port;
              NetProto.buf_int b body.my_port;
          )
          (fun s pos ->
              let ip, pos = NetProto.get_ip s pos in
              let port, pos =  NetProto.get_int s pos in
              let my_port, _ =  try NetProto.get_int s pos with _ -> 0, pos in
              {
                ip =  ip;
                port = port;
                my_port = my_port;
              })
          (fun body ->
              Printf.sprintf "IDENTIFIED { \n\tip = %s\n\tport = %d\n\tmy_port = %d\n  }\n"
                (Ip.to_string body.ip) body.port body.my_port)

      end

    let my_port = ref 0
    let set_my_port port = my_port := port
    let my_id = ref (Sha1.random ())
    let set_my_id id = my_id := id
    let get_my_id () = !my_id

    let message_handlers = ref StringMap.empty

    let peers_by_id = ref Sha1Map.empty
    let nmessages = ref 0

    let declare_handler m handler =
      message_handlers := StringMap.add m.message_name (Obj.magic
          (handler : peer connection -> 'a -> unit) )
      !message_handlers


    let write_message m t =

      if not (debug level_DDD flag_P) then () else lprintf "SENDING %s:\n" m.message_name;
      if not (debug level_DDD flag_P) then () else lprintf "%s\n" (m.message_printer t);

      LittleEndian.buf_string31 send_buf m.message_name;
      m.message_writer send_buf t


    let netstring_of_message m t =
      Buffer.clear send_buf;
      LittleEndian.buf_int31 send_buf 0;
      LittleEndian.buf_int31 send_buf !nmessages;
      incr nmessages;
      write_message m t;
      let s = Buffer.contents send_buf in
      let len = String.length s - 4 in
      LittleEndian.str_int31 s 0 len;
      s

    let do_handler c m msg =
      match
        try
          Some (StringMap.find m.message_name !message_handlers)
        with Not_found -> None
      with
        None ->
          if not (debug level_D flag_P) then () else lprintf "Warning: no handler for %s" m.message_name
      | Some f ->
          try
            f c msg
          with e ->
              if not (debug level_D flag_P) then () else lprintf "Exception %s in handler for %s\n"
                (Printexc2.to_string e) m.message_name

(* Note: receiver_parse_message won't raise an exception unless something bad happened during parsing ! *)
    let receiver_parse_message c msg pos =
      let _message_counter = LittleEndian.get_int31 msg pos in
      let pos = pos + 4 in
      begin
        match c.conn_peer with
          None ->
            lprintf "No peer associated with connection"
        | Some p ->
            if not (debug level_DDD flag_P) then () else lprintf "New message from peer %s"
              (Sha1.to_string (M.peer_id p))
      end;
      let (m, msg) = parse_message msg pos in
      do_handler c m msg

    let read_cut_messages c b nread =
      let rec iter () =
        if b.len >= 4 then
          let msg_len, pos = LittleEndian.get_int31 b.buf b.pos in
          if b.len >= 4 + msg_len then
            let s = String.sub b.buf pos msg_len in
            buf_used b (msg_len + 4);

            receiver_parse_message c s 0;
            iter ()
      in
      iter ()

    let client_reader c b nread =
      read_cut_messages c b nread

    let rec parse_string c s pos len =
      if len >= 4 then
        let msg_len, new_pos = LittleEndian.get_int31 s pos in
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
      client_reader c (TcpSocket.buf sock) nread

    let sock_closer c _ reason  =
      (match c.conn_sock with
          None -> ()
        | Some sock ->
            c.conn_sock <- None;
            TcpSocket.close sock reason);
      (match c.conn_peer with
          None -> ()
        | Some p ->
            (match M.peer_connection p with
                Some cc when cc == c ->
                  M.set_peer_connection p None;
                  do_handler c ByeMsg.msg {
                    ByeMsg.from_state = c.conn_state
                  }
              | _ -> ());
            c.conn_peer <- None
      );
      c.conn_state  <- DISCONNECTED

    let sock_connected c sock =
      c.conn_state <- CONNECTED


    let connection_send c msg content =
      match c.conn_sock with
        None -> failwith "NetP2P.send: peer disconnected"
      | Some sock ->
          begin
            match c.conn_peer with
              None -> ()
            | Some p ->
                if not (debug level_DDD flag_P) then () else lprintf "Sending message to peer %s"
                  (Sha1.to_string (M.peer_id p))
          end;
          TcpSocket.write_string sock (netstring_of_message msg content)

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
      TcpSocket.set_reader sock (sock_reader c);
      TcpSocket.set_closer sock (sock_closer c);
      TcpSocket.set_rtimeout sock 30;
      TcpSocket.set_lifetime sock 120;
      TcpSocket.set_connected sock (sock_connected c);
      send_id c;
      c

    let bind_port port =
      let event_handler t event =
        match event with
          TcpServerSocket.CONNECTION (s, Unix.ADDR_INET(from_ip, from_port)) ->

            let from_ip = Ip.of_inet_addr from_ip in
            lprintf "New Incoming Connecting from %s:%d"
              (Ip.to_string from_ip) from_port;

            let token = create_token unlimited_connection_manager in
            let sock = TcpSocket.create
                token "client-helper connection" s in
            let _c = init_connection sock None from_ip from_port in
            ()
        | _ -> ()

      in
      TcpServerSocket.create "server"
        Unix.inet_addr_any port event_handler

    let disconnect p =
      match M.peer_connection p with
        None -> ()
      | Some c ->
          lprintf "NetP2P.disconnect called";
          sock_closer c () Closed_by_user

(* TODO: garbage collect !peers_by_id *)
    let new_peer peer_id addr =
      try
        let p = Sha1Map.find peer_id !peers_by_id in
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
          peers_by_id := Sha1Map.add peer_id p !peers_by_id;
          (p : M.peer)

    let connect_addr ip port peer =
      let token = create_token unlimited_connection_manager in
      let sock = TcpSocket.create_new token "connect_to" in
      TcpSocket.connect sock ip port;
      let c = init_connection sock peer ip port in
      c

    let connect p =
      match M.peer_connection p with
      | Some conn ->
          if not (debug level_D flag_P) then () else lprintf "NetP2P.connect: peer already connected"
      | None ->
          match M.peer_address p with
            None ->
              failwith "NetP2P.connect: peer address is unknown"
          | Some (ip,port) ->
              lprintf "Connecting to %s %s:%d"
                (Sha1.to_string (M.peer_id p)) (Ip.to_string ip) port;
              let c = connect_addr ip port (Some p) in
              M.set_peer_connection p ( Some c )

    let set_rtimeout c n =
      match c.conn_sock with
        None -> ()
      | Some sock -> TcpSocket.set_rtimeout sock n

    let set_lifetime c n =
      match c.conn_sock with
        None -> ()
      | Some sock -> TcpSocket.set_lifetime sock n

    let set_max_output_buffer c n =
      match c.conn_sock with
        None -> ()
      | Some sock -> TcpSocket.set_max_output_buffer sock n

    let connection_identified c =
      c.conn_state <- IDENTIFIED;
      set_rtimeout c 7200;
      let (ip, port) = c.conn_addr in
      connection_send c IdentifiedMsg.msg {
        IdentifiedMsg.ip = ip;
        port = port;
        my_port = !my_port;
      }

    let set_rtimeout p n =
      match M.peer_connection p with
        None -> ()
      | Some c -> set_rtimeout c n

    let set_lifetime p n =
      match M.peer_connection p with
        None -> ()
      | Some c -> set_lifetime c n

    let set_max_output_buffer p n =
      match M.peer_connection p with
        None -> ()
      | Some c -> set_max_output_buffer c n

    let _ =
      declare_handler ConnectingMsg.msg (fun
          (c :  peer connection) m ->
          let (p : M.peer) = new_peer m.ConnectingMsg.id (Some c.conn_addr) in
          match c.conn_peer with
            Some (pp : M.peer) when p != pp ->
              if not (debug level_D flag_P) then () else lprintf "connectingMsg: unexpected peer"
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
                      lprintf "Connecting to already IDENTIFIED peer: disconnecting";
                      sock_closer c () Closed_by_user
                  | _ ->
                      lprintf "Connecting to already connected peer: disconnecting";
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
                lprintf "FOR message %s" m.message_name;
                lprintf "No peer attached for message: disconnecting";
                sock_closer c () (Closed_for_error "no peer attached")
          else begin
                lprintf "FOR message %s" m.message_name;
              lprintf "Peer not identified for message: disconnecting";
              sock_closer c ()  (Closed_for_error "not identified")
            end
      )

    let send p msg content =
      match M.peer_connection p with
        None ->
          if not (debug level_DDD flag_P) then () else lprintf "While sending %s:\n" msg.message_name;
          if not (debug level_DDD flag_P) then () else lprintf "%s\n" (msg.message_printer content);

          failwith "NetP2P.send: peer not connected"
      | Some c ->
          if c.conn_state = IDENTIFIED then
            connection_send c msg content


    let get_peer id = Sha1Map.find id !peers_by_id
    let iter_peers f =
      Sha1Map.iter (fun _ p -> f p) !peers_by_id


  end



end

include I

 *)
