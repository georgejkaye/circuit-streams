open Belnap
open Streams

type mealy = {
    inputs: int;
    outputs: int;
    states: int;
    initial: int;
    (* T : S -> M -> S * N *)
    transition_function: ((int * value list) * (value list * int)) list;
}

let stream_to_mealy cs =
    let rec compute_from_state seen transitions cur =
        let next_transitions = get_next_outputs_and_derivatives cur in
        let (new_seen, new_transitions, frontier) = 
            List.fold_left (
                fun (new_seen, new_transitions, frontier) -> 
                    fun (values, output, derv) ->
                        match List.find_opt (stream_equality derv) new_seen with
                            | None -> (
                                derv :: new_seen, 
                                (values, output, derv) :: new_transitions, 
                                derv :: frontier
                            )
                            | Some s' -> (
                                new_seen, 
                                (values, output, s') :: new_transitions, 
                                frontier
                            )
            ) 
            (seen, [], []) 
            next_transitions
        in
        let transitions = (cur, new_transitions) :: transitions in
        let (seen, transitions) = 
            List.fold_left 
                (fun (seen, transitions) -> 
                    fun cur -> 
                        compute_from_state seen transitions cur
                ) 
                (new_seen, transitions) 
                frontier 
            in
        (seen, transitions)
    in 
    let (seen, mealy_data) = (compute_from_state [cs] [] cs) in
    let seen = List.rev seen in
    let transition_fn = List.fold_left 
        (fun transitions -> fun (state, data) -> 
            let state_transition = 
                List.map 
                    (fun (inputs, outputs, transition) ->
                        ((state, inputs), (outputs, transition))
                    )
                    data 
            in
            transitions @ state_transition
        )
        []
        mealy_data
    in
    let state_map = List.mapi (fun i -> fun cs -> (cs, i)) seen in
    let updated_transition_fn = List.map 
        (fun ((s, m), (n, t)) -> 
            (((List.assoc s state_map), m), (n, (List.assoc t state_map)))
        ) transition_fn in
    {
        inputs = cs.inputs;
        outputs = cs.inputs;
        initial = 0;
        states = List.length mealy_data;
        transition_function = updated_transition_fn 
    }


let mealy_to_string mm = 
    let each_state = List.map 
        (fun ((s, m), (n, t)) -> 
            let current = string_of_int s in
            let input = value_list_to_string m in
            let output = value_list_to_string n in
            let next = string_of_int t in
            "s" ^ current ^ " | " ^
            input ^ " || " ^
            output ^ " | " ^
            "s" ^ next
        ) 
        mm.transition_function
    in 
    List.fold_left 
        (fun acc -> fun cur -> acc ^ cur ^ "\n") 
        "" 
        each_state






(* in {
    inputs = cs.inputs;
    outputs = cs.outputs;
    states = 
} *)