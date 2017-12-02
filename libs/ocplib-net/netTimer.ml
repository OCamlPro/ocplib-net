
type t = Lwt_timeout.t option ref

let create n f =
  let x = ref None in
  let rec create_timeout () =
    let t = Lwt_timeout.create n
                               (fun () ->
                                 match !x with
                                 | None -> ()
                                 | Some t ->
                                    f ();
                                    create_timeout ()
                               )
    in
    x := Some t;
    Lwt_timeout.start t;
  in
  create_timeout ();
  x

let stop x =
  match !x with
  | None -> ()
  | Some t ->
     x := None;
     Lwt_timeout.stop t

let current_time = ref (Unix.gettimeofday ())
let () =
  let (_t : t) = create
                   1 (fun () ->
                     current_time := Unix.gettimeofday ())
  in
  ()

let current_time () = !current_time
