open Belnap
open Approximant
open Helpers

(**  
    Causal stream functions are defined for each tick i by using
    approximants f_i : M^i -> N.

    circuit_stream = (inputs, outputs, approximant function)
*)
type circuit_stream = {
    behaviour: int -> approximant
}

let output_at cs xs i = 
    let approximant = cs.behaviour i in
    eval_func approximant xs

let simulate cs xs = 
    List.map (fun i -> output_at cs xs i) (nats (List.length xs))
