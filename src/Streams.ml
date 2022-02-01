open Belnap
open Approximant
open Helpers

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

let get_nth_behaviour cs i = 
    if i < List.length cs.prefix_behaviour then
        List.nth cs.prefix_behaviour i
    else 
        let i = i - List.length cs.prefix_behaviour in
        let i = i mod List.length cs.period_behvaiour in
        List.nth cs.period_behvaiour i

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

let initial_output cs a = 
    eval_func empty_history (get_nth_behaviour cs 0) [a]


let stream_derivative cs a = 
    let initial = initial_output cs a in
    let evalf j = List.mapi (fun i -> fun f -> partial_eval (get_history_up_to cs (i + j)) a initial (i + j) f) in
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
        name = cs.name ^ "{" ^ (value_list_to_string a) ^ "}";
        inputs = cs.inputs;
        outputs = cs.outputs;
        prefix_behaviour = new_prefix_evaled;
        period_behvaiour = new_period_evaled;
    }



let output_at cs xs i = 
    let approximant = get_nth_behaviour cs i in
    let history = get_history_up_to cs i in
    eval_func history approximant xs

let simulate cs xs = 
    List.map (fun i -> output_at cs xs i) (nats (List.length xs))

let stream_to_string_list cs i =
    let behaviours = List.map (get_nth_behaviour cs) (nats i) in
    List.mapi (func_to_string cs.name) behaviours

let print_stream cs i = 
    let strings = stream_to_string_list cs i in
    List.fold_left (fun _ -> fun cur -> print_endline cur) () strings