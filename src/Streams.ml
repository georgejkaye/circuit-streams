open Belnap
open Helpers

type func = 
    | Value of value
    (* Arg(i,j) is the jth element of the input at tick i *)
    | Arg of int * int          
    | Apply of gate * func
    | Par of func list

let rec outputs = function
    | Value _ -> 1
    | Arg _ -> 1
    | Apply _ -> 1
    | Par fs -> List.length fs

let rec typecheck = function
    | Value _ -> true
    | Arg _ -> true
    | Apply (g, f) -> 
        typecheck f && outputs f == gate_inputs g
    | Par fs -> List.for_all typecheck fs

let depends_on f = 
    let rec depends_on' = function
        | Value _ -> []
        | Arg (i, j) -> [i]
        | Apply (g, f) -> depends_on' f
        | Par fs -> List.concat (List.map depends_on' fs)
    in
    List.concat (List.map depends_on' f)

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

type approximant = int -> func list

(**  
    Causal stream functions are defined for each tick i by using
    approximants f_i : M^i -> N.

    In the case of circuit stream functions, there is a finite
    prefix of approximants followed by a finite recurring period
    of approximants.

    Therefore we specify them as (prefix, period)
*)
type circuit_stream = int * int * approximant list * approximant list

let stream_inputs (cs : circuit_stream) = match cs with
| (m, _, _, _) -> m

let stream_outputs (cs : circuit_stream) = match cs with
| (_, n, _, _) -> n

let initial_value (cs : circuit_stream) =
    let (_, _, prefix, period) = cs in
    if prefix == [] then
        if period == [] then failwith "[initial_value] empty stream"
                        else List.hd period
                    else 
                        List.hd prefix 

let stream_derivative (cs : circuit_stream) =
    let (inputs, outputs, prefix, period) = cs in
    if prefix == [] then     
        if period == [] then (inputs, outputs, [], []) 
                        else (inputs, outputs, [], List.tl period @ [List.hd period])
                    else (inputs, outputs, List.tl prefix, period)

let is_causal (cs : circuit_stream) =
    let (_, _, prefix, period) = cs in
    let approximants = prefix @ period in
    let dependencies = List.mapi (fun i -> fun f -> 
        let dependencies = depends_on (f i) in
        if List.length dependencies == 0 then None else Some (max_element dependencies)
    ) approximants in
    print_list_def dependencies (fun d -> match d with | None -> "_" | Some d -> string_of_int d);
    List.for_all2 (fun d -> fun i -> match d with | None -> true | Some d ->  d <= i) dependencies (nats (List.length dependencies))

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