
let should_exit = ref false

let signal = ref None
let main () =
  let rec sleep () =
    if !should_exit then
      Lwt.return ()
    else
      let t,u = Lwt.wait () in
      signal := Some u;
      Lwt.bind t sleep
  in
  Lwt_main.run (sleep ())

let wakeup_main_thread () =
  match !signal with
  | None -> ()
  | Some u ->
     signal := None;
     Lwt.wakeup u ()

let exit () =
  should_exit := true;
  wakeup_main_thread ()

let defer f =
  Lwt.async (fun () -> Lwt.bind (Lwt.pause ()) f)

let wakeup u =
  defer (fun () -> Lwt.wakeup u (); Lwt.return ())
