
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
