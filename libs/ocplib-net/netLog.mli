(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Small layer over Lwt_log *)

val debug :
  ?exn:exn ->
  ?section:Lwt_log.section ->
  ?location:string * int * int ->
  ?logger:Lwt_log.logger -> ('a, unit, string, unit) format4 -> 'a
val info :
  ?exn:exn ->
  ?section:Lwt_log.section ->
  ?location:string * int * int ->
  ?logger:Lwt_log.logger -> ('a, unit, string, unit) format4 -> 'a
val notice :
  ?exn:exn ->
  ?section:Lwt_log.section ->
  ?location:string * int * int ->
  ?logger:Lwt_log.logger -> ('a, unit, string, unit) format4 -> 'a
val warning :
  ?exn:exn ->
  ?section:Lwt_log.section ->
  ?location:string * int * int ->
  ?logger:Lwt_log.logger -> ('a, unit, string, unit) format4 -> 'a
val error :
  ?exn:exn ->
  ?section:Lwt_log.section ->
  ?location:string * int * int ->
  ?logger:Lwt_log.logger -> ('a, unit, string, unit) format4 -> 'a
val fatal :
  ?exn:exn ->
  ?section:Lwt_log.section ->
  ?location:string * int * int ->
  ?logger:Lwt_log.logger -> ('a, unit, string, unit) format4 -> 'a
