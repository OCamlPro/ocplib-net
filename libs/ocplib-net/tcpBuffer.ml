let min_buffer_read = 16000

type t = {
    mutable buf : string;
    mutable pos : int;
    mutable len : int;
    mutable max_buf_size : int;
  }

(* BufferWriteOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow of int * int * int

exception BufferReadOverflow of int * int

let new_string len =
  BytesCache.get len

let release_string s =
  BytesCache.putback s

let create max =
  {
    buf = new_string min_buffer_read;
    pos = 0;
    len = 0;
    max_buf_size = max;
  }

let prepare_space_for_bytes b n =
  let curpos = b.pos + b.len in
  let len = String.length b.buf in
  if curpos + n < len then curpos
  else
    if b.len + n > b.max_buf_size then
      raise (BufferWriteOverflow (b.len, n, b.max_buf_size))
    else
      let old_buf = b.buf in

      if b.len + n > len then begin
          let new_len = min (max (2 * b.len) (b.len + n)) b.max_buf_size  in
          (* Printf.eprintf "alloc  %d\n%!" new_len; *)
          let new_buf = new_string new_len in
          b.buf <- new_buf
        end;
      String.blit old_buf b.pos b.buf 0 b.len;
      if b.buf != old_buf then
        release_string old_buf;
      b.pos <- 0;
      b.len

let add_bytes_from_string b s pos len =
  let curpos = prepare_space_for_bytes b len in
  (*
  Printf.eprintf "prepare_space_for_bytes (write): %d/%d/%d -> %d %d\n%!"
                 b.pos b.len (String.length b.buf) curpos len;
   *)
  String.blit s pos b.buf curpos len;
  b.len <- b.len + len

let release_bytes b nused =
  (* Printf.eprintf "release_bytes %d\n%!" nused; *)
  b.len <- b.len - nused;
  b.pos <- b.pos + nused

let length b = b.len
let max_refill t = t.max_buf_size - t.len
let can_refill t = t.max_buf_size > t.len

let release b =
  release_string b.buf;
  b.buf <- ""

let add_bytes_from_read b fd n =
  let curpos = prepare_space_for_bytes b n in
  (* Printf.eprintf "prepare_space_for_bytes (read): %d/%d/%d -> %d %d\n%!"
                 b.pos b.len (String.length b.buf) curpos n; *)
  Lwt.bind (Lwt.catch
              (fun () ->
                (* Printf.eprintf "read: %d[%d,%d]\n%!"
                               (String.length b.buf) curpos n; *)
                Lwt_unix.read fd b.buf curpos n)
              (fun exn ->
                match exn with
                | Lwt.Canceled ->
                   Lwt.return (-1)
                | Unix.Unix_error(Unix.ECONNRESET,_,_) -> (* CLOSED *)
                   Lwt.return 0
                | _ ->
                   Printf.eprintf "Lwt_unix.read: exception %s\n%!"
                                  (Printexc.to_string exn);
                   Lwt.return 0
           ))
           (fun nread ->
             (* Printf.eprintf "add_bytes_from_read PARTIAL\n%!"; *)
             if nread >= 0 then
               b.len <- b.len + nread;
             Lwt.return nread
           )

let get b pos =
  if b.len <= pos then
    raise (BufferReadOverflow (pos, b.len));
  b.buf.[b.pos + pos]

let blit b pos0 s pos len =
  if b.len < pos0 + len then
    raise (BufferReadOverflow (pos0+len, b.len));
  (* Printf.eprintf "String.blit %d/%d/%d %d/%d/%d\n%!"
                 (String.length b.buf) b.pos b.len
                 (String.length s) pos len; *)
  String.blit b.buf b.pos s pos len

let read b s pos len =
  blit b 0 s pos len;
  release_bytes b len

let write b fd =
  Lwt.bind (Lwt.catch
              (fun () -> Lwt_unix.write fd b.buf b.pos b.len)
              (fun exn ->
                Lwt.return
                  (match exn with
                   | Unix.Unix_error(Unix.EPIPE, _, _) -> 0
                   | Lwt.Canceled -> -1

                   | exn ->
                      Printf.eprintf "Lwt_unix.write: exception %s\n%!"
                                     (Printexc.to_string exn);
                      0)
              ))
           (fun nwritten ->
             (* Printf.eprintf "write PARTIAL\n%!"; *)
             if nwritten > 0 then
               release_bytes b nwritten;
             Lwt.return nwritten
           )

let ( _ : Sys.signal_behavior ) =
  Sys.signal Sys.sigpipe Sys.Signal_ignore
