
open NetTypes

let () =

  let t = TcpClientSocket.connect
            ()
            (Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 40_000))
            (fun t event ->
              Printf.eprintf "client: event %s\n%!"
                             (TcpClientSocket.string_of_event event);
              match event with
              | `RTIMEOUT
                | `WTIMEOUT -> ()
              | `READ_DONE _nread ->
                 let s = TcpClientSocket.read_string t in
                 Printf.printf "Received: %S\n%!" s;
                 TcpClientSocket.close t Closed_by_user
              | `CONNECTED -> ()
              | `CAN_REFILL -> ()
              | `WRITE_DONE -> ()
              | `CLOSED _reason ->
                 exit 0
            )
  in
  TcpClientSocket.write_string t "Hello!!!\r\n";
  Printf.eprintf "Client started\n%!";
  NetLoop.main ()
