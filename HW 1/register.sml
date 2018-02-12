(* Define a record to represent an instance of this machine's state *)
{a=0, b=0};

(* Define the type of the machine state, {a:int, b:int} *)
type MachineState = {a:int, b:int};

(* Function to create a new instance of the machine state *)
fun newMachine() : MachineState = {a=0, b=0};

(* Create a machine state (stored in 'it') *)
newMachine ();

(* Rewrite the constructor function to provide initial values *)
fun newMachine (a, b) : MachineState = {a=a, b=b};

(* Getter function for 'a' and 'b' registers *)
fun getA (m : MachineState) = #a m;
fun getB (m : MachineState) = #b m;

(* Test if it works *)
newMachine(4,5);
getA it;
getB it;

(* Setter function *)
fun setA ({a = oldA, b = oldB}, newA) = {a = newA, b = oldB}

(* Ideally, it would look like... *)
val m1 = newMachine (0,0);
val m2 = setA (m1, 100);

(* Modified setter function *)
fun setA({b = oldB, ...}, newA) = {a = newA, b = oldB};

(* Setter function explicitly typed as MachineState *)
fun setA ({b = oldB, ...} : MachineState, newA) = {a = newA, b = oldB} : MachineState;

(* Setter function calling the newMachine function in the body *)
fun setA ({b = oldB, ...} : MachineState, newA) = newMachine (newA, oldB);

(* Something more readable *)
fun setA (m : MachineState, newA) = newMachine (newA, #b m);
fun setB (m: MachineState, newB) = newMachine (#a m, newB);

(* Define instruction data type *)
datatype Instruction = SetA of int | SetB of int | Add | Sub | Disp;

(* Check to see if the symbols are members of Instructions *)
Add;
Sub;
Disp;
SetA;
SetB;

(* SetA and SetB are functions that given an int return an Instruction *)
SetA 100;

(* Display an Instruction constructor at the command prompt *)
val instr = Add;

(* Function to convert an Instruction into a string *)
fun i2s (SetA a) = "SetA " ^ Int.toString(a)
  | i2s (SetB b) = "SetA " ^ Int.toString(b)
  | i2s Add = "Add"
  | i2s Sub = "Sub"
  | i2s Disp = "Disp"
  ;

(* Check to see if it worked *)
i2s instr;
print (i2s instr);

(* Make a list, bind it to a name *)
[SetA 10, SetB 2, Add, SetB 4, Sub, Disp];
val prog = it;

(* hd function - returns head of the list *)
hd prog;

(* tl function - get everything, but the first element *)
tl prog;

(* Empty list can be represented as '[]' or nil *)
[];
nil;

(* Specify the type of a list *)
[] : Instruction list;

(* Evaluate Instructions *)
fun eval (m : MachineState, SetA a) : MachineState = setA (m, a)
 | eval (m : MachineState, SetB b) : MachineState = setB (m, b)
 | eval (m : MachineState, Add ) : MachineState = setA (m, #a m + #b m)
 | eval (m : MachineState, Sub ) : MachineState = setB (m, #a m - #b m)
 | eval (m : MachineState, Disp ) : MachineState =
    (print (Int.toString(#a m) ^ "\n"); m)
 ;

(* ML-ish way to write it *)
fun run (m: MachineState, [] : Instruction list) = m
  | run (m: MachineState, prog : Instruction list) =
    let val instr = hd prog          (* next instruction in prog list *)
        val instrs = tl prog         (* rest of prog list *)
        val _ = print (i2s instr ^ "\n");
        val m1 = eval (m, instr);
    in
       run (m1, instrs)                 (* recursive call *)
    end;


(* Test Display Function *)
val m = newMachine (0,0);
val prog = [SetA 10, SetB 2, Add, SetB 4, Sub ,Disp];
run (m, prog);
