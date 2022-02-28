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

let get_output_ports c =
    Array.map (fun (port,_,_) -> port) c.outputs

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

let traverse f acc c =
    let rec traverse' seen frontier f acc = 
        match frontier with
            | [] -> acc
            | port :: frontier -> 
        let acc = f acc port in
        let (seen, frontier) = match port with 
            | Block b -> 
                if List.mem b.id seen 
                    then 
                        (seen, frontier) 
                    else 
                        let ports = b.ports in
                        let nexts = Array.to_list (Array.map (fun (p,_) -> p) ports) in
                        (b.id :: seen, nexts @ frontier)
            | Input _ -> (seen, frontier)
            | Circuit (c, i) -> (seen, (get_output_ports c).(i) :: frontier)
            | Value _ -> (seen, frontier)
        in
        traverse' seen frontier f acc
    in
    traverse' [] (Array.to_list (get_output_ports c)) f acc

let get_inputs c =
    1 + traverse (fun acc -> function
        | Input i -> max acc i
        | _ -> acc
    ) 0 c

let get_outputs c = Array.length c.outputs

let increment_inputs k c =
    traverse (fun _ -> function
        | Block b ->   
            (List.fold_left 
                (fun _ -> fun i -> match b.ports.(i) with
                    | (Input j, d) -> b.ports.(i) <- (Input (j+k), d)
                    | _ -> ()
                ) () (nats_of b.ports)
            )
        | _ -> ())
    ()
    c

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

let iterate id c x m n inputs outputs = 
    let (id, c1) = c id in
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