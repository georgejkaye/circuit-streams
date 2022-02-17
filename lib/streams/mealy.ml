open Logic.Values
open Functions
open Helpers.Help
open Logic.Order
open Table

type mealy = {
    inputs: int;
    outputs: int;
    states: int;
    initial: int;
    (* T : S -> M -> S * N *)
    transition_function: ((int * belnap_value list) * (belnap_value list * int)) list;
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
        outputs = cs.outputs;
        initial = 0;
        states = List.length mealy_data;
        transition_function = updated_transition_fn 
    }

let assign_state_values mm = 
    let determine_state_order = 
        let get_order_from_state n = 
            let possible_inputs = all_inputs_of_length (mm.inputs) in
            let next_states = List.map (fun x -> (x, snd (List.assoc (n, x) mm.transition_function))) possible_inputs in
            derive_order_from_existing (value_list_order (mm.inputs)) next_states
        in
        let initial_order = {
            elements = nats mm.states;
            order    = List.init mm.states (fun x -> (x, [])) 
        } in
        List.fold_left 
        (fun acc -> fun cur ->
            let new_order = get_order_from_state cur in
            let comborder = combine_orders acc new_order in
            comborder

        )
        initial_order
        (nats mm.states)
    in
    let generate_value_tuples order =
        let generate_single_tuple n = 
            List.map 
                (fun s -> if (po_lte order s n) then Both else Non)
                (nats mm.states)
        in List.map (fun i -> (i, generate_single_tuple i)) (nats mm.states)
    in
    generate_value_tuples determine_state_order

let mealy_to_truth_tables mm ord =
    let (outputs_list, transition_list) = 
        List.fold_left 
        (fun (outputs_list, transition_list) -> 
            fun ((s,m),(n,t)) -> 
                let current_assignment = List.assoc s ord in
                let next_assignment = List.assoc t ord in
                let inputs = Array.of_list (current_assignment @ m) in
                let outputs = Array.of_list n in
                let transitions = Array.of_list next_assignment in
                ((inputs, outputs) :: outputs_list, (inputs, transitions) :: transition_list)
        )
        ([], [])
        mm.transition_function
    in
    let inputs = mm.inputs + mm.states in
    let rows = List.length mm.transition_function in
    ({
        inputs = inputs;
        outputs = mm.outputs;
        rows = rows;
        matrix = Array.of_list outputs_list;
    },
    {
        inputs = inputs;
        outputs = mm.states;
        rows = rows;
        matrix = Array.of_list transition_list;
    })
    

(* Printer *)

let mealy_to_string mm = 
    let each_state = List.map 
        (fun ((s, m), (n, t)) -> 
            let current = string_of_int s in
            let input = belnap_value_list_to_string m in
            let output = belnap_value_list_to_string n in
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