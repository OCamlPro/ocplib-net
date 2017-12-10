
type t = string

let compare = String.compare
let equal = String.equal

let char_hex n =
  Char.unsafe_chr (n + if n < 10 then Char.code '0' else (Char.code 'a' - 10))

let of_string d =
  let len = String.length d in
  let result = Bytes.create (len*2) in
  for i = 0 to len-1 do
    let x = Char.code d.[i] in
    Bytes.unsafe_set result (i*2) (char_hex (x lsr 4));
    Bytes.unsafe_set result (i*2+1) (char_hex (x land 0x0f));
  done;
  Bytes.unsafe_to_string result

let to_string s =
  let len = String.length s in
  if len mod 2 <> 0 then invalid_arg "Hex.to_string";
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _ -> raise (Invalid_argument "Hex.to_string")
  in
  let byte i = digit s.[i] lsl 4 + digit s.[i+1] in
  let result = Bytes.create (len/2) in
  for i = 0 to len/2 - 1 do
    Bytes.set result i (Char.chr (byte (2 * i)));
  done;
  Bytes.unsafe_to_string result
