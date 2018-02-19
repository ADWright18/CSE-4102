(* Explicit Continuations *)
fun add3 x y z c =
  add2 x y (fn s => add2 s z c);

add3 1 2 3 id;

(* List-Oriented Enviornment Implementation *)
exception NameNotBound;

fun env_new () = nil; (* returns an empty list *)

fun env_bind name value store = ((name, value) :: store);

fun env_lookup name [] = raise NameNotBound
  | env_lookup name ((n, value) :: rest) =  if name = n
                                            then value
                                            else env_lookup name rest

val store = env_bind "y" 5 (env_bind "x" 10 (env_new()));

(* Functional Environment Implementation *)
fun env_new () : string -> int = fn x => raise NameNotBound;
fun env_bind env name v : string -> int
  = fn n => if n = name
            then v
            else (env n);

val env = env_bind (env_bind (env_new ()) "x" 10) "y" 5;
