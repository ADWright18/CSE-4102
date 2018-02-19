(* Define Environment Type *)
exception NameNotBound of string;
type Env = (string * int) list

(* Create a new empty environment *)
fun env_new () = [] : Env;

(* Create a bind function. env_bind is immutable (it's contents does not change) *)
val env_bind = fn env : Env =>
fn name : string =>
fn value : int =>
(name, value) :: env : Env;

val someName = "Error";

(* When accessing a tuple, store it in a variable and use #1, #2,... to get the values *)
fun env_lookup ([] : Env) = raise NameNotBound someName
  | env_lookup env : Env => fn name : string =>
    let val pair = hd env              (* next instruction in prog list *)
        val rest = tl env             (* rest of prog list *)
    in
        if name = #1 pair
        then #2 pair
    else
        env_lookup (rest)               (* recursive call *)
    end;
