open NetTypes

let count = ref 0
let verbose = ref false
let port = 41_000

let rec iter_create my_port dst_port r =
  let sockaddr = Sockaddr.loopback my_port in
  let udp_handler t event =
    if !verbose then
      Printf.eprintf "udp_handler: event %s\n%!"
                     (UdpSocket.string_of_event event);
    match event with
    | `READ_DONE ->
       UdpSocket.read_packets
         t
         (fun u ->
           if !verbose then
             Printf.eprintf "Received %S from %s\n%!"
                            u.msg_content
                            (Sockaddr.to_string u.msg_sockaddr);
           incr count;
           if !count mod 1000 = 0 then
             Printf.eprintf "Count: %d\n%!" !count;
           UdpSocket.write t "Merci !" u.msg_sockaddr
         )
    | _ -> ()
  in
  let t = UdpSocket.create r sockaddr udp_handler in
  UdpSocket.write t "Hello !" (Sockaddr.loopback dst_port);
  ()

let () =
  let my_port = ref 40000 in
  let dst_port = ref 40000 in
  Arg.parse [
      "--server", Arg.Unit (fun () ->
                      my_port := 40_000;
                      dst_port := 40_001), " Act as server (40000 -> 40001)";
      "--client", Arg.Unit (fun () ->
                      my_port := 40_001;
                      dst_port := 40_000), " Act as client (40001 -> 40000)";
    ] (fun _ -> assert false) "";
  let r = ref true in
  iter_create !my_port !dst_port r;
  NetLoop.main ()
