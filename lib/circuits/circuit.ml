open Logic.Values

type block = {
    (* Each input to the gate is delayed by some n *)
    ports: (input_type * int) list;
    gate: gate;
} and input_type = 
    | Block of block
    | Input of int

let rec evaluate_block i vss b =
    let inputs = 
        List.map 
            (fun (it, d) -> 
                if i < d then Non else match it with    
                | Block b -> evaluate_block (i - d) vss b
                | Input j -> 
                    if i - d >= List.length vss then Non else 
                    List.nth (List.nth vss (i - d)) j
            )
            (b.ports)
    in
    eval_gate b.gate inputs

type circuits = {
  outputs: (block * int) list
}

let evaluate_circuit i vss c = 
    List.map
        (fun (b,d) -> (evaluate_block (i-d) vss b))
        c.outputs