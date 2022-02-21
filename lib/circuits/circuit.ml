open Helpers.Help
open Logic.Values

type block = {
    id : int;
    (* Each input to the gate is delayed by some n *)
    ports: (port * int) array;
    gate: gate;
} and port = 
    | Block of block
    | Input of int
    | Circuit of circuit * int
    | Value of belnap_value
and circuit = {
    input_names: string array;
    output_names: string array;
    outputs: (port * int) array
}

let gates c = 
    let rec gates' seen c =
        let rec gates'' seen = function
            | Block b -> if List.mem b.id seen then seen else 
                let seen = (b.id :: seen) in
                Array.fold_left (fun seen -> fun (cur, _) -> gates'' seen cur) seen b.ports
            | Input _ -> seen
            | Circuit (c, _) -> gates' seen c 
            | Value _ -> seen
        in
        Array.fold_left (fun seen -> fun (cur, _) -> gates'' seen cur) seen c.outputs
    in
    List.length (gates' [] c)

let rec set_circuit_inputs c ins = 
    let rec set_port_inputs = function
        | Block b -> Block (set_block_inputs b)
        | Input i -> ins.(i)
        | Circuit (c, j) -> let c = set_circuit_inputs c ins in Circuit (c, j) 
        | Value v -> Value v
    and set_block_inputs b = 
        {
            id = b.id;
            ports = Array.map (fun (p, d) -> (set_port_inputs p, d)) b.ports;
            gate = b.gate
        }   
    in
    {
        input_names = c.input_names;
        output_names = c.output_names;
        outputs = Array.map (fun (p, d) -> (set_port_inputs p, d)) c.outputs
    }

(**
    Evaluate a port

    Arguments:
    - lookup: the lookup table for the blocks in this circuit
    - acc: the accumulator containing the previous ports in this context
    - i: the tick at which to evaluate
    - vss: the list of lists of input values at each tick
    - p: the port to evaluate
    - d: the delay applied to this port

    Returns:
    - The lookup table filled with additionally calculated blocks
    - The evaluated list of values
*)
let rec evaluate_port lookup i vss p d =
    if i < d 
    then 
        (lookup, Non)
    else 
        match p with    
        | Block b -> 
            let (lookup, v) = lookup_block lookup (i - d) vss b in 
            (lookup, v)
        | Input j -> 
            let v = 
                if i - d >= Array.length vss 
                then 
                    Non 
                else 
                    let inputs_at_tick = vss.(i - d) in
                    inputs_at_tick.(j) 
            in 
            (lookup, v)
        | Circuit (c, j) -> 
            let (lookup, outputs) = evaluate_circuit lookup i vss c 
            in (lookup, outputs.(j))
        | Value v -> (lookup, v)
and evaluate_block lookup i vss b =
    let (lookup, evaled_ports) = 
        Array.fold_left_map 
            (fun lookup -> fun (p, d) -> evaluate_port lookup i vss p d) 
            lookup
            b.ports
    in
    (lookup, eval_gate b.gate evaled_ports)
and lookup_block lookup i vss b = 
    if i < 0 then (lookup, Non) else
        match lookup.(b.id).(i) with 
            | Some v -> (lookup, v)
            | None -> 
                let (lookup, evaled) = evaluate_block lookup i vss b in
                lookup.(b.id).(i) <- Some evaled;
                (lookup, evaled)
and evaluate_circuit lookup i vss c = 
    Array.fold_left_map
        (fun lookup -> fun (p, d) -> evaluate_port lookup i vss p d)
        lookup
        c.outputs
and begin_evaluation i vss c =
    let lookup = Array.make_matrix (gates c) i None in 
    let (_, outputs) = evaluate_circuit lookup i vss c in
    outputs

let simulate_circuit n inputs c =
    let lookup = Array.make_matrix (gates c) n None in
    let (_, outputs) = 
        Array.fold_left_map
            (fun lookup -> fun i -> evaluate_circuit lookup i inputs c)
            lookup
            (array_nats n)
    in
    outputs


let circuit_simulation_to_string n inputs c = 
    let print_header_section names = array_to_string names "" "" " " id
    in
    let input_header = print_header_section c.input_names in
    let output_header = print_header_section c.output_names in
    let header = input_header ^ " | " ^ output_header in
    print_endline header;
    let line = String.init (String.length header) (fun _ -> '-') in
    print_endline line;
    let outputs = simulate_circuit n inputs c in
    let print_row i = 
        let print_section vs names =
            let lengths = Array.map (String.length) names in
            let num = Array.length lengths in
            let print_cell v n = (belnap_value_to_string v) ^ String.init (n - 1) (fun _ -> ' ') in  
            list_to_string (nats num) "" "" " " (fun i -> print_cell vs.(i) lengths.(i))
        in
        let input_section = print_section inputs.(i) c.input_names in
        let output_section = print_section outputs.(i) c.output_names in
        input_section ^ " | " ^ output_section
    in
    if n == 0 then () else
        let first = print_row 0 in 
            if n == 1 then print_endline first else 
                List.fold_left
                    (fun _ -> fun cur -> print_endline (print_row cur))
                    ()
                    (nats n)    