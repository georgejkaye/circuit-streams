open Belnap
open Streams

type mealy = {
    inputs: int;
    outputs: int;
    states: int;
    transition_function: ((int * value list) * int) list;
    output_function: ((int * value list) list) list
}

let stream_to_mealy cs =
    let rec compute_from_state seen cur =
        let seen = cur :: seen in
        (* print_string "acc is [";
        List.fold_left (fun acc -> fun cur -> print_short_stream cur; print_string " ") () seen;
        print_endline "]"; *)
        let derivatives = compute_all_stream_derivatives cur in
        let predicate cs = not (List.exists (fun x -> stream_equality x cs) seen) in
        let new_states = List.filter predicate derivatives in
        (* print_string "next states are [";
        List.fold_left (fun acc -> fun cur -> print_short_stream cur; print_string " ") () new_states;
        print_endline "]"; *)
        List.fold_left (fun acc -> fun cur -> compute_from_state acc cur) seen new_states
    in compute_from_state [] cs





(* in {
    inputs = cs.inputs;
    outputs = cs.outputs;
    states = 
} *)