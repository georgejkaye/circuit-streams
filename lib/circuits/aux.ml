open Helpers.Nats

open Core

let get_output_names c =
    Array.map (fun (_,_,name) -> name) c.outputs

let get_output_ports c =
    Array.map (fun (port,_,_) -> port) c.outputs

(**
    Get the number of gates in a circuit
    @param c The circuit
    @return The number of gates
*)

let traverse f acc c =
    let rec traverse' seen frontier f acc = 
        match frontier with
            | [] -> acc
            | port :: frontier -> 
        let (acc, seen, frontier) = match port with 
            | Block b -> 
                if List.mem b.id seen 
                    then 
                        (acc, seen, frontier) 
                    else 
                        let acc = f acc port in
                        let ports = b.ports in
                        let nexts = Array.to_list (Array.map (fun (p,_) -> p) ports) in
                        (acc, b.id :: seen, nexts @ frontier)
            | Input _ -> (f acc port, seen, frontier)
            | Circuit (c, i) -> (f acc port, seen, (get_output_ports c).(i) :: frontier)
            | Value _ -> (f acc port, seen, frontier)
        in
        traverse' seen frontier f acc
    in
    traverse' [] (Array.to_list (get_output_ports c)) f acc

let gates c = 
    traverse (fun acc -> function 
        | Block _ -> acc + 1
        | _ -> acc 
    ) 0 c

let get_inputs c =
    1 + traverse (fun acc -> function
        | Input i -> max acc i
        | _ -> acc
    ) 0 c

let get_outputs c = Array.length c.outputs

(**
    Increment all the inputs in this circuit by some constant. 
    Useful for putting circuits in parallel.

    @param k the number to increment the inputs by
    @param c

*)
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