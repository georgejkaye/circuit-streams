open Logic.Values

type block = {
  (* Each input to the gate is delayed by some n *)
  inputs: (input_type * int) list;
  gate: gate;
} and input_type = 
  | Block of block
  | Input of int

type circuits = {
  outputs: (block * int) list
}