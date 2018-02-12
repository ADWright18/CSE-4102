(* Define the type of the machine state, {a:int, b:int} *)
type MachineState = {a:int, b:int};

(* Rewrite the constructor function to provide initial values *)
fun newMachine (a, b) : MachineState = {a=a, b=b};

(* Something more readable *)
fun setA (m : MachineState, newA) = newMachine (newA, #b m);
fun setB (m: MachineState, newB) = newMachine (#a m, newB);

(* Define instruction data type *)
datatype Instruction = SetA of int | SetB of int | Add | Sub | Disp;

(* Function to convert an Instruction into a string *)
fun i2s (SetA a) = "SetA " ^ Int.toString(a)
  | i2s (SetB b) = "SetA " ^ Int.toString(b)
  | i2s Add = "Add"
  | i2s Sub = "Sub"
  | i2s Disp = "Disp"
  ;

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
