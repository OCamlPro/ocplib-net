(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Either an Unix.ADDR_INET or Unix.ADDR_UNIX *)
type t = Unix.sockaddr =
       | ADDR_UNIX of string
       | ADDR_INET of Unix.inet_addr * int

let loopback port =
  Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port)

let any port = Unix.ADDR_INET(Unix.inet_addr_any, port)

let to_string = NetUtils.string_of_sockaddr
let of_ip ip port =
  Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
let of_file filename =
  Unix.ADDR_UNIX filename

let of_hostname hostname port =
  try
    let h = Unix.gethostbyname hostname in
    let l = Array.map (fun addr ->
                Unix.ADDR_INET(addr, port)) h.Unix.h_addr_list
    in
    Array.to_list l
  with _ -> []

let of_string name port =
  try [of_ip name port]
  with _ -> of_hostname name port
