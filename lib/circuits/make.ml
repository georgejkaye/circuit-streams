open Helpers.Nats

open Core
open Aux

let combine circuits outputs input_names =
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

let iterate id c x m n inputs outputs = 
    let (id, c1) = c id in
    let (id, c2) = c id in
    let (id, c3) = c id in
    (id, combine
        [| 
            (c1, Array.init (x + m) (fun i -> if i < x then Value Non else Input (i - x))) ;
            (c2, Array.init (x + m) (fun i -> if i < x then Circuit (c1, i) else Input (i - x))) ;
            (c3, Array.init (x + m) (fun i -> if i < x then Circuit (c2, i) else Input (i - x))) 
        |]
        (Array.init n (fun i -> (2, i + x, 0, outputs.(i))))
        inputs
    )

let compare cs =
    let ((_, hd_circ), tl) = match cs with
        | [] -> failwith "[compare] No circuits to compare"
        | (hd :: tl) -> (hd, tl)
    in 
    let input_length = get_inputs hd_circ in
    let output_length = get_outputs hd_circ in
    List.fold_left
        (fun _ -> fun (_, cur_circ) -> 
            if input_length == get_inputs cur_circ && output_length == get_outputs cur_circ 
                then () 
                else failwith "[compare] Not all circuits have the same inputs and outputs"
        )
        ()
        tl
    ;
    let outputs = Array.concat (
        List.map
            (fun (name, circ) -> 
                let outputs = circ.outputs in
                Array.map (fun (p,d,s) -> (p,d,name ^ s)) outputs
            ) 
            cs)
    in
    {
        input_names = hd_circ.input_names;
        outputs = outputs;
    }