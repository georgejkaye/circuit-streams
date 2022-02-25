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
    outputs: (port * int * string) array
}

let get_output_names c =
    Array.map (fun (_,_,name) -> name) c.outputs

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
        Array.fold_left (fun seen -> fun (cur, _, _) -> gates'' seen cur) seen c.outputs
    in
    List.length (gates' [] c)

let get_output_port c i = 
    let (output, _, _) = c.outputs.(i) in
    output

let combine_circuits circuits outputs input_names =
    let get_actual_port_and_delay output = 
        let (circuit_number, number, delay, name) = output in
        let (actual_circuit, _) = circuits.(circuit_number) in
        let (port, new_delay, _) = actual_circuit.outputs.(number) in
        (port, delay + new_delay, name)
    in
    let replace_inputs circ replacements = 
        let rec replace_inputs' seen frontier =
            if frontier == [] then () else 
                let (p, _) = List.hd frontier in
                let frontier = if List.length frontier > 1 then List.tl frontier else [] in
                let (seen, frontier) =
                    match p with
                        | Block b -> 
                            if List.mem b.id seen then 
                                (seen, frontier) 
                            else
                                let frontier = List.fold_left
                                    (fun frontier -> fun i -> 
                                        match b.ports.(i) with 
                                            | (Input j, d) -> 
                                                let replacement = replacements.(j) in
                                                b.ports.(i) <- (replacement, d);
                                                frontier
                                            | p -> p :: frontier
                                    ) 
                                    frontier
                                    (nats_of b.ports)
                                in (b.id :: seen, frontier)
                        | Circuit (c,i) -> 
                            let (port,delay,_) = c.outputs.(i) in
                            (seen, (port,delay) :: frontier)
                        | Value _ -> (seen, frontier)
                        | Input _ -> (seen, frontier)
                in
                replace_inputs' seen frontier
        in
        replace_inputs' [] (Array.to_list (Array.map (fun (p,d,_) -> (p,d)) circ.outputs))
    in
    Array.iter (fun (circ, inputs) -> replace_inputs circ inputs) circuits;
    let outputs = Array.map get_actual_port_and_delay outputs
    in
    {
        input_names = input_names;
        outputs = outputs
    }

let iterate c x m n inputs outputs = 
    let (id, c1) = c 0 in
    let (id, c2) = c id in
    let (id, c3) = c id in
    (id, combine_circuits
        [| 
            (c1, Array.init (x + m) (fun i -> if i < x then Value Non else Input (i - x))) ;
            (c2, Array.init (x + m) (fun i -> if i < x then Circuit (c1, i) else Input (i - x))) ;
            (c3, Array.init (x + m) (fun i -> if i < x then Circuit (c2, i) else Input (i - x))) 
        |]
        (Array.init n (fun i -> (2, i + x, 0, outputs.(i))))
        inputs
    )


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
        (fun lookup -> fun (p, d, _) -> evaluate_port lookup i vss p d)
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
    let output_header = print_header_section (Array.map (fun (_,_,name) -> name) c.outputs) in
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
        let output_section = print_section outputs.(i) (Array.map (fun (_,_,name) -> name) c.outputs) in
        input_section ^ " | " ^ output_section
    in
    if n == 0 then () else
        let first = print_row 0 in 
            if n == 1 then print_endline first else 
                List.fold_left
                    (fun _ -> fun cur -> print_endline (print_row cur))
                    ()
                    (nats n)    