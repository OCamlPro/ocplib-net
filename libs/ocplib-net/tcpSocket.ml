
open NetTypes

type event = tcpSocketEvent

type t = {
    fd : Unix.file_descr;

    mutable rtimeout: float;
    mutable next_rtimeout : float;

    mutable wtimeout: float;
    mutable next_wtimeout : float;

    mutable lifetime : float;

    mutable event_handler : handler;
    mutable error : close_reason;

    mutable name : string;
    born : float;
    mutable can_close : bool;
  }

and handler = t -> event -> unit



(* val create : name:string -> Unix.file_descr -> handler -> t *)
let create ~name _ = assert false
(* val connect: string -> Unix.file_descr -> int -> handler -> t *)
let connect _ = assert false
(* val write : t -> string -> pos:int -> len:int -> unit *)
let write _ = assert false
(* val close : t -> close_reason -> unit*)
let close _ = assert false
(* val shutdown : t -> close_reason -> unit *)
let shutdown _ = assert false
(* val set_rtimeout : t -> float -> unit *)
let set_rtimeout _ = assert false
(* val set_wtimeout : t -> float -> unit *)
let set_wtimeout _ = assert false
(* val set_lifetime : t -> float -> unit *)
let set_lifetime _ = assert false
(* val set_handler : t -> handler -> unit *)
let set_handler _ = assert false
(* val handler : t -> handler *)
let handler _ = assert false
(* val closed : t -> bool *)
let closed _ = assert false
