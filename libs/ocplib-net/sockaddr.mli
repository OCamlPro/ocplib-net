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

(* Server-side addresses *)

(* [loopback port] returns an address on the loopback interface (127.0.0.1). *)
val loopback : int -> t

(* [any port] returns an address on the default network interface. *)
val any : int -> t

(* Client-side addresses *)

(* [of_string hostname port] returns a list of addresses corresponding
   to the given hostname. An empty list means that the name could not
   be found. The hostname can be a direct IP address. *)
val of_string : string -> int -> t list

(* [of_ip ip port] returns the address of the corresponding IP address *)
val of_ip : string -> int -> t

(* [of_hostname hostname port] returns a list of addresses corresponding
   to the given hostname. An empty list means that the name could not
   be found.*)
val of_hostname : string -> int -> t list

(* Local client/server *)

(* [of_file name] returns an address on the file [name]. *)
val of_file : string -> t



(* pretty-print an address *)
val to_string : t -> string
