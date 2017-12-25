(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

  let find_slot n =
    let rec find_slot n i =
      if n = 0 then i else
        find_slot (n/2) (i+1)
    in
    find_slot n 0

  let buffers = Array.make 64 []

  let get n =
    let i = find_slot (n-1) in
    let m = 1 lsl i in
    match buffers.(i) with
    | hd :: tl ->
      buffers.(i) <- tl;
      assert (Bytes.length hd >= n);
      hd
    | [] -> Bytes.create m

  let putback b =
    let n = Bytes.length b in
    if n >= 4096 then
      let i = find_slot (n-1) in
      if List.length buffers.(i) < 4 then
        buffers.(i) <- b :: buffers.(i)
