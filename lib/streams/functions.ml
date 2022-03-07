open Helpers.Nats
open Logic.Values
open Logic.Approximant

(**  
    Causal stream functions are defined for each tick i by using
    approximants f_i : M^i -> N.

    circuit_stream = (inputs, outputs, approximant function)
*)
type circuit_stream = {
    name: string;
    inputs: int;
    outputs: int;
    prefix_behaviour: approximant list;
    period_behvaiour: approximant list
}

(* Accessing approximants *)

let defined_behaviour cs = 
    (List.length cs.prefix_behaviour) + (List.length cs.period_behvaiour)

let get_nth_behaviour cs i = 
    if i < List.length cs.prefix_behaviour then
        List.nth cs.prefix_behaviour i
    else 
        let i = i - List.length cs.prefix_behaviour in
        let i = i mod List.length cs.period_behvaiour in
        List.nth cs.period_behvaiour i

let get_n_behaviours cs i =
    List.map (get_nth_behaviour cs) (nats i)

let empty_history = {
    size = 0;
    history = [];
}

let get_history_up_to cs i = 
    let hist = List.map (get_nth_behaviour cs) (revnats i) in
    {
        size = List.length hist;
        history = hist
    }

(* Operations on streams *)

let initial_output cs a = 
    eval_func empty_history (get_nth_behaviour cs 0) [| a |]


let stream_derivative cs a = 
    let initial = initial_output cs a in
    let evalf j = List.mapi (fun i -> fun f -> partial_eval a initial (i + j) f) in
    let prefix_length = List.length cs.prefix_behaviour in
    let old_period = if prefix_length < 1 then 
        let hd = List.hd cs.period_behvaiour in
        List.tl cs.period_behvaiour @ [hd] 
        else cs.period_behvaiour
    in
    let new_prefix = (List.tl cs.prefix_behaviour) @ old_period in
    let new_prefix_evaled = evalf 1 new_prefix in
    let new_period_evaled = evalf ((List.length new_prefix) + 1) (cs.period_behvaiour) in
    {
        name = cs.name ^ "{" ^ (belnap_value_array_to_string a) ^ "}";
        inputs = cs.inputs;
        outputs = cs.outputs;
        prefix_behaviour = new_prefix_evaled;
        period_behvaiour = new_period_evaled;
    }

let get_next_outputs_and_derivatives cs =
    List.map (fun vs -> (vs, initial_output cs vs, stream_derivative cs vs)) (all_inputs_of_width cs.inputs)

let tick cs a = (initial_output cs a, stream_derivative cs a)

(* Stream comparison *)

let stream_equality cs ds = 
    let longest = max (defined_behaviour cs) (defined_behaviour ds) in
    let csb = get_n_behaviours cs longest in
    let dsb = get_n_behaviours ds longest in
    List.fold_left2 
        (fun acc -> fun c -> fun d -> acc &&
            List.fold_left 
                (fun acc -> fun i -> acc && func_equality c.func.(i) d.func.(i))
                true
                (nats (Array.length c.func))
        )
        true
        csb
        dsb

(* Simulating streams *)

let simulate cs xs = 
    List.rev (fst (
        List.fold_left 
            (fun (outputs , func) -> fun cur -> 
                let (initial, derivative) = tick func cur in
                (initial :: outputs, derivative)    
        )  ([], cs) xs
    ))


(* Printers *)

let stream_to_string_list i cs =
    let behaviours = List.map (get_nth_behaviour cs) (nats i) in
    List.mapi (func_to_string cs.name) behaviours

let print_short_stream cs = print_string cs.name

let print_stream i cs = 
    let strings = stream_to_string_list i cs in
    List.fold_left (fun _ -> fun cur -> print_endline cur) () strings