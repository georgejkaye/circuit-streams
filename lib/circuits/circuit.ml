open Helpers.Help
open Logic.Values

type block = {
    id : int;
    (* Each input to the gate is delayed by some n *)
    ports: (input_type * int) list;
    gate: gate;
} and input_type = 
    | Block of block
    | Input of int

let rec evaluate_block lookup i vss b =
    let inputs = 
        List.rev (snd 
            (List.fold_left 
                (fun (lookup, acc) -> fun (it, d) -> 
                    if i < d then (lookup, Non :: acc) else 
                        match it with    
                        | Block b -> 
                            let (lookup, v) = lookup_block lookup (i - d) vss b in 
                            (lookup, v :: acc)
                        | Input j -> 
                            let v = if i - d >= List.length vss then Non else 
                            List.nth (List.nth vss (i - d)) j
                            in (lookup, v :: acc)
                )
                (lookup, [])
                (b.ports)
            )
        )
    in
    eval_gate b.gate inputs
and lookup_block lookup i vss b = 
    if i < 0 then (lookup, Non) else
        match lookup.(b.id).(i) with 
            | Some v -> (lookup, v)
            | None -> 
                let evaled = evaluate_block lookup i vss b in
                lookup.(b.id).(i) <- Some evaled;
                (lookup, evaled)

type circuits = {
    gates: int;
    input_names: string list;
    output_names: string list;
    outputs: (input_type * int) list
}

let evaluate_circuit i vss c = 
    let lookup = Array.make_matrix c.gates i None in
    List.rev (snd (List.fold_left
        (fun (lookup, acc) ->
            fun (b, d) ->
            match b with 
                | Block b -> let (lookup, value) = lookup_block lookup (i-d) vss b in (lookup, value :: acc)
                | Input j -> (lookup, (List.nth (List.nth vss (i-d)) j) :: acc)
        )
        (lookup, [])
        c.outputs
    ))

let simulate_circuit n inputs c =
    List.map
        (fun i -> evaluate_circuit i inputs c)
        (nats n)

let circuit_simulation_to_string n inputs c = 
    let print_header_section names = 
        let m = List.length names in
        if m == 0 then "" else 
            let first = List.hd names in
            if m == 1 then first else
                List.fold_left (fun acc -> fun cur -> acc ^ " " ^ cur) first (List.tl names) 
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
            let lengths = List.map (String.length) names in
            let num = List.length lengths in
            let print_cell v n = (belnap_value_to_string v) ^ String.init (n - 1) (fun _ -> ' ') in  
            if num == 0 then "" else 
                let first = print_cell (List.hd vs) (List.hd lengths) in
                if num == 1 then first else 
                    List.fold_left2
                        (fun acc -> fun v -> fun n -> acc ^ " " ^ (print_cell v n))
                        first
                        (List.tl vs) 
                        (List.tl lengths)
        in
        let input_section = print_section (List.nth inputs i) c.input_names in
        let output_section = print_section (List.nth outputs i) c.output_names in
        input_section ^ " | " ^ output_section
    in
    if n == 0 then () else
        let first = print_row 0 in 
            if n == 1 then print_endline first else 
                List.fold_left
                    (fun _ -> fun cur -> print_endline (print_row cur))
                    ()
                    (nats n)    