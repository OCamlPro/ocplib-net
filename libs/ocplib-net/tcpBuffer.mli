
type t

(* BufferOverflow (previous_len, added_len, max_len) *)
exception BufferWriteOverflow of int * int * int
exception BufferReadOverflow of int * int

val create : int -> t
(* Release unused buffer *)
val release : t -> unit

val add_bytes_from_string : t -> string -> int -> int -> unit
val add_bytes_from_read :
  t -> Lwt_unix.file_descr -> int -> int Lwt.t

(* Release a given number of bytes *)
val release_bytes : t -> int -> unit

(* Number of bytes in buffer *)
val length : t -> int

(* Maximal number of bytes that could be written *)
val max_refill : t -> int
val can_refill : t -> bool

val get : t -> int -> char

(* bytes read are NOT released *)
val blit : t -> int -> string -> int -> int -> unit
(* bytes read are immediately released *)
val read : t -> string -> int -> int -> unit

val write : t -> Lwt_unix.file_descr -> int Lwt.t
