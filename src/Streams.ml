open Belnap
open Approximant

(**  
    Causal stream functions are defined for each tick i by using
    approximants f_i : M^i -> N.

    circuit_stream = (inputs, outputs, approximant function)
*)
type circuit_stream = {
    behaviour: int -> approximant
}

let output_at cs = cs.behaviour

(* let stream_inputs (cs : circuit_stream) = match cs with
| (m, _, _, _) -> m

let stream_outputs (cs : circuit_stream) = match cs with
| (_, n, _, _) -> n

let initial_value (cs : circuit_stream) =
    let (_, _, prefix, period) = cs in
    if prefix == [] then
        if period == [] then failwith "[initial_value] empty stream"
                        else List.hd period
                    else 
                        List.hd prefix 

let stream_derivative (cs : circuit_stream) =
    let (inputs, outputs, prefix, period) = cs in
    if prefix == [] then     
        if period == [] then (inputs, outputs, [], []) 
                        else (inputs, outputs, [], List.tl period @ [List.hd period])
                    else (inputs, outputs, List.tl prefix, period)

let is_causal (cs : circuit_stream) =
    let (_, _, prefix, period) = cs in
    let approximants = prefix @ period in
    let dependencies = List.mapi (fun i -> fun f -> 
        let dependencies = depends_on (f i) in
        if List.length dependencies == 0 then None else Some (max_element dependencies)
    ) approximants in
    print_list_def dependencies (fun d -> match d with | None -> "_" | Some d -> string_of_int d);
    List.for_all2 (fun d -> fun i -> match d with | None -> true | Some d ->  d <= i) dependencies (nats (List.length dependencies))

(* Evaluate a circuit stream up to a given tick *)
let eval (cs : circuit_stream) (k : int) (s : value list list) = 
    let rec eval' i cs k sigma =    
        let hd = initial_value cs in
        let hd_eval = eval_func_list (hd i) sigma in
        if k == 0 then 
            [hd_eval]
        else
            let tl = stream_derivative cs in
            hd_eval :: eval' (i+1) tl (k-1) sigma
in eval' 0 cs k s *)