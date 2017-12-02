
open NetTypes

let () =
  let nconnections = ref 0 in
  let t = TcpServerSocket.create ~name:"server"
      (fun t event ->
        Printf.eprintf "server: event %s\n%!"
                       (TcpServerSocket.string_of_event event);
        match event with
        | `CONNECTION _ ->
          incr nconnections;
          if !nconnections = 2 then
            TcpServerSocket.close t Closed_by_user
        | _ -> ()
      )
  in
  Printf.eprintf "Server started on port %d\n%!" (TcpServerSocket.port t);
  NetLoop.main ()
