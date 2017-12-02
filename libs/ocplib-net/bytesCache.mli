
  (* [get n] gets a buffer of at least [n] bytes. The final length [m]
     such that [m >= n] will be a power of 2. *)
  val get : int -> bytes

  (* [putback b] when [b] will not be used anymore *)
  val putback : bytes -> unit
