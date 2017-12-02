
(* Test with in several terminals:
╰─➤ cat /dev/zero | netcat 127.0.0.1 40000
 *)

open NetTypes

let () =
  let nconnections = ref 0 in
  let t = TcpServerSocket.create ~name:"server"
                                 ~port:40_000
      (fun t event ->
        Printf.eprintf "server: event %s\n%!"
                       (TcpServerSocket.string_of_event event);
        match event with
        | `CONNECTION (fd, addr) ->
          incr nconnections;
          if !nconnections = 6 then
            TcpServerSocket.close t Closed_by_user
          else
            let id = !nconnections in
            let counter = ref 0 in
            let t = TcpClientSocket.create
                       ~name:"client" fd
                       (fun t event ->
                         Printf.eprintf "(%d) client: event %s\n%!"
                                        id
                                        (TcpClientSocket.string_of_event event);
                         match event with
                         | `READ_DONE n ->
                            (* Printf.eprintf "write_string ...\n%!"; *)
                            counter := !counter + n;
                            TcpClientSocket.write_string
                              t
                              (Printf.sprintf "READ %d\n" !counter);
                            (* Printf.eprintf "read_string ...\n%!";*)
                            let _s = TcpClientSocket.read_string t in
                            (* Printf.eprintf "read_string DONE ...\n%!"; *)
                            ()
                         | `RTIMEOUT ->
                            TcpClientSocket.close t Closed_for_timeout
                         | _ -> ()
                       )
            in
            TcpClientSocket.set_rtimeout t 5.;
            ()
        | _ -> ()
      )
  in
  Printf.eprintf "Server started on port %d\n%!" (TcpServerSocket.port t);
  NetLoop.main ()
