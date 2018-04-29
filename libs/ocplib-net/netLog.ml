(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let debug = Lwt_log.ign_debug_f
let info = Lwt_log.ign_info_f
let notice = Lwt_log.ign_notice_f
let warning = Lwt_log.ign_warning_f
let error = Lwt_log.ign_error_f
let fatal = Lwt_log.ign_fatal_f
