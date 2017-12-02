
(* TODO: use let (t,u) = Lwt.wait() in Lwt.run t; ... Lwt.wakeup u *)
let should_exit = ref false
let main () =
  let rec sleep () =
      (* Printf.eprintf "_%!"; *)
    Lwt.bind (Lwt_unix.sleep 0.1)
      (fun () ->
        if not !should_exit then sleep () else Lwt.return ())
  in
  Lwt_main.run (sleep ())

let exit () =
    (* Printf.eprintf "should exit\n%!"; *)
  should_exit := true
