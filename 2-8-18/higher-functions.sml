# What is commong to all this?

fun sum nil = 0
  | sum(a::b) = a + sum b;

fun prod nil = 1
  | prod (a::b) = a * prod b

# Tail recursive
fun rev nil l = l
  | rev (a::b) l = rev b (a::l)

# Tail recursive
fun member x nil = false
  | member x (a::b) = x=a orselse member x b;

(* Folding in ML *)
fun foldr f e nil = e
  | foldr f e (a::b) = f a  (foldr f e b)

fun foldl f e nil = e
  | foldl f e (a::b) = foldl f (f e a) b;

(* Revisiting Sum *)
fun sum l = foldr (op +) 0 l;
(* Take 2 *)
fun sum l = foldl (op +) 0 l;

(* Another One *)
fun ex1 l = foldr (op ::) nil l;
(* Example 2 *)
fun ex2 l = foldl (op ::) nil l;

(* More Examples *)
fun map f nil = nil
  | map f (a::b) = (f a)::(map f b);

fun toUpper s =
  | implode (map Char.toUpper (explode s));

(* Filter *)
fun filter p nil = nil
  | filter p (a::b) = if p a
                      then a::(filter p b)
                      else filter p b;

fun odd l = filter (fn x => x mod 2=1) l;

(* Continuations *)
fun add3 x y z = add2 (add2 x y) z;
val s = add3 1 2 3;

(* Recursive Sum *)
fun sum 0 = 0
  | sum n = add 2 n (sum (n - 1));

(* More recursion *)
fun rec () = rec ();
