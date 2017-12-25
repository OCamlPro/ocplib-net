(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val system :
  string ->
  (Unix.process_status -> 'a) ->
  unit

val exec :
  string ->
  string array ->
  ?timeout:float ->
  ?stdin:string ->
  ?stdout:string ->
  ?stderr:string ->
  (Unix.process_status -> 'a) -> (* W_EXITED 99 means an exception *)
  unit
