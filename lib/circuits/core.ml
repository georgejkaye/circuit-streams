open Logic.Values
open Logic.Gates

(**
    A block is a logic gate with some inputs, which
    may or may not be delayed
*)
type block = {
    (* These should be unique and are used for memoisation *)
    id : int;
    (* (p,i) means port p delayed by i ticks *)
    ports: (port * int) array;
    gate: gate;
} 

(**
    A port is what an input to a gate can be
*)
and port = 
    | Block of block
    (** Input(i) is the ith input, starting at 0 *)
    | Input of int
    (** Circuit(c, i) is the ith output of circuit c *)
    | Circuit of circuit * int
    | Value of belnap_value
and circuit = {
    input_names: string array;
    (** Output (p,i,s) is port p, delayed for n ticks, with name s *)
    outputs: (port * int * string) array
}