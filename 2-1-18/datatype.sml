(* Data Type *)
datatype Money = Dollars of int | Cents of int;
Cents 10;
Dollars 8;
[Dollars 5, Cents 12];

(* Money list *)
fun value [] = 0.0
  | value (Dollars d :: rest) = real(d) + value rest
  | value (Cents c :: rest) = real(c) / 100.0 + value rest
  ;
val value = fn : Money list -> real

value [Dollars 10, Cents 14, Dollars 3, Cents 12];

(* Binary Tree, the 'a is a type *)
datatype 'a Tree = Leaf
  | Node of 'a * 'a Tree * 'a Tree;

(* Insertion in a tree *)
fun emptyTree () = Leaf;

fun addTree e Leaf = Node(e, Leaf, Leaf)
  | addTree e (Node(v,l,r)) =
    if (e < v)
    then Node(v, addTree e l, r)
    else if (e > v)
      then Node(v, l, addTree e r)
      else raise AlreadyPresent;

(* Exception Handling *)
addTree 1 (addTree 3 (addTree 2 (addTree 3 (emptyTree ()))))
  handle AlreadyPresent
  => (print "Attempt to add an element twice\n"; Leaf);

(* Expression Data Type *)
datatype Expr = Int of in
  | Var of string
  | Plus of Expr * Expr
  | Times of Expr * Expr
  | Opposite of Expr

val e0 = Plus(Var "x", Times (Var "y", Int 3));
