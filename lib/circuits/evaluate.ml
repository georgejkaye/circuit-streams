open Helpers.Help
open Logic.Values
open Logic.Gates

open Core
open Aux

(**
    Evaluate a port

    @param lookup the lookup table for the blocks in this circuit
    @param acc the accumulator containing the previous ports in this context
    @param i the tick at which to evaluate
    @param vss the list of lists of input values at each tick
    @param p the port to evaluate
    @param d the delay applied to this port

    @return The lookup table filled with additionally calculated blocks
    @return The evaluated list of values
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

let simulate_circuit n inputs c =
    let lookup = Array.make_matrix (gates c) n None in
    let (_, outputs) = 
        Array.fold_left_map
            (fun lookup -> fun i -> evaluate_circuit lookup i inputs c)
            lookup
            (array_nats n)
    in
    outputs

let string_of_simulation n inputs c = 
    let print_header_section names = array_to_string names "" "" " " id
    in
    let input_header = print_header_section c.input_names in
    let output_header = print_header_section (Array.map (fun (_,_,name) -> name) c.outputs) in
    let header = input_header ^ " | " ^ output_header in
    let line = String.init (String.length header) (fun _ -> '-') in
    let outputs = simulate_circuit n inputs c in
    let print_row i = 
        let print_section vs names =
            let lengths = Array.map (String.length) names in
            let num = Array.length lengths in
            let print_cell v n = (belnap_value_to_string v) ^ String.init (n - 1) (fun _ -> ' ') in  
            list_to_string (nats num) "" "" " " (fun i -> print_cell vs.(i) lengths.(i))
        in
        let input_section = print_section inputs.(i) c.input_names in
        let output_section = print_section outputs.(i) (get_output_names c) in
        input_section ^ " | " ^ output_section
    in
    print_endline header;
    print_endline line;
    if n == 0 then () else
        let first = print_row 0 in 
            if n == 1 then print_endline first else 
                List.fold_left
                    (fun _ -> fun cur -> print_endline (print_row cur))
                    ()
                    (nats n)    