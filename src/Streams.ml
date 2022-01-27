open Belnap
open Print

type func = 
    | Value of value
    (* Arg(i,j) is the jth element of the input at tick i *)
    | Arg of int * int          
    | Apply of gate * func
    | Par of func list

let rec func_to_string = function
    | Value v -> value_to_string v
    | Arg (i, j) -> "σ(" ^ (string_of_int i) ^ ")[" ^ (string_of_int j) ^ "]"
    | Apply (g, f) -> (gate_to_string g) ^ "(" ^ (func_to_string f) ^ ")"
    | Par fs -> list_to_string fs "(" ")" ", " func_to_string

let rec eval_func f xs = match f with
    | Value v -> [v]
    | Arg (i, j) -> [List.nth (List.nth xs i) j]
    | Apply (g, f) -> 
        let f = eval_func f xs in 
        [eval_gate g f]
    | Par fs -> 
        List.concat (List.map (fun f -> eval_func f xs) fs)

let eval_func_list fs xs = List.concat (List.map (fun f -> eval_func f xs) fs)

(**  
    Causal stream functions are defined for each tick i by using
    approximants f_i : M^i -> N.

    In the case of circuit stream functions, there is a finite
    prefix of approximants followed by a finite recurring period
    of approximants.

    Therefore we specify them as (prefix, period)
*)
type circuit_stream = (int -> func list) list * (int -> func list) list

let initial_value cs =
    let (prefix, period) = cs in
    if prefix == [] then
        if period == [] then failwith "[initial_value] empty stream"
                        else List.hd period
                    else 
                        List.hd prefix 

let stream_derivative cs =
    let (prefix, period) = cs in
    if prefix == [] then     
        if period == [] then ([], []) 
                        else ([], List.tl period @ [List.hd period])
                    else (List.tl prefix, period)

(* Evaluate a circuit stream up to a given tick *)
let eval (cs : circuit_stream) (k : int) (s : value list list) = 
    let rec eval' i cs k sigma =    
        let hd = initial_value cs in
        let hd_eval = eval_func_list (hd i) sigma in
        if k == 0 then 
            [hd_eval]
        else
            let tl = stream_derivative cs in
            hd_eval :: eval' (i+1) tl (k-1) sigma
in eval' 0 cs k s