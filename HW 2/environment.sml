(* Example Environment *)
fun env_lookup "x" = 10
  | env_lookup "y" = 2
  | env_lookup "z" = 2
  | env_lookup "w" = 2
  | env_lookup "v" = 2
  ;

exception NameNotBound of string;
type Env = ? (* I'll describe this type below *)
val env_new = fn : unit -> Env
val env_bind = fn : Env -> string -> int -> Env
val env_lookup = fn : Env -> string -> int
