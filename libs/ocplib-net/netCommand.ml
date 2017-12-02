
let system cmd cont =
  Lwt.async (fun () ->
    Lwt.bind (Lwt_process.exec (Lwt_process.shell cmd))
      (fun s -> cont s; Lwt.return ()))

let exec cmd args ?timeout ?stdin ?stdout  ?stderr cont =
  let stdin = match stdin with
    | None -> None
    | Some filename ->
      let fd = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
      Some (`FD_move fd)
  in
  let stdout = match stdout with
    | None -> None
    | Some filename ->
      let fd = Unix.openfile filename
        [Unix.O_TRUNC; Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
      Some (`FD_move fd)
  in
  let stderr = match stderr with
    | None -> None
    | Some filename ->
      let fd = Unix.openfile filename
        [Unix.O_TRUNC; Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
      Some (`FD_move fd)
  in
  Lwt.async (fun () ->
    Lwt.bind (
      Lwt.catch
        (fun () -> Lwt_process.exec ?timeout ?stdin ?stdout ?stderr (cmd, args))
        (fun exn ->
           Printf.eprintf "Process %S raised exception %S\n%!"
             cmd (Printexc.to_string exn);
           Lwt.return (Unix.WEXITED 99))
    )
      (fun s -> cont s; Lwt.return ()))
