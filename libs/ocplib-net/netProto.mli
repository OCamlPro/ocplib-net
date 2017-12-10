
val get_int8 : string -> int -> int * int
val get_int16 : string -> int -> int * int
val get_int31 : string -> int -> int * int
val get_int32 : string -> int -> int32 * int

val buf_int8 : Buffer.t -> int -> unit
val buf_int16 : Buffer.t -> int -> unit
val buf_int31 : Buffer.t -> int -> unit
val buf_int32 : Buffer.t -> int32 -> unit

val get_string8 : string -> int -> string * int
val get_string16 : string -> int -> string * int
val get_string31 : string -> int -> string * int

val buf_string8 : Buffer.t -> string -> unit
val buf_string16 : Buffer.t -> string -> unit
val buf_string31 : Buffer.t -> string -> unit

val str_int31 : bytes -> int -> int -> unit
