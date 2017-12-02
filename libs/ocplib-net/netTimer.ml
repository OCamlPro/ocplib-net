
type t = Lwt_timeout.t

let create n f =
  let t = Lwt_timeout.create n f in
  Lwt_timeout.start t;
  t

let stop = Lwt_timeout.stop
