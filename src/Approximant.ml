open Belnap
open Helpers

type func = 
    | Value of value
    (* Input(i,j) is the jth element of the input at tick i *)
    | Input of int * int          
    | Apply of gate * func
    | Par of func list


type approximant_func =
    | Value of value
    | Input of int * int
    | Apply of gate * approximant_func
    | Par of approximant_func list
    (* Approx(i, j) is the jth element of the ith approximant *)
    | Approx of int * int

type approximant = {
    inputs: int;
    outputs: int;
    current: int;
    history : approximant list;
    func: approximant_func list;
}

let get_past_approximant af n = List.nth af.history (af.current - n - 1)

let rec outputs history = function
    | Value _ -> 1
    | Input _ -> 1
    | Apply _ -> 1
    | Par fs -> List.length fs
    | Approx (i, j) -> 
        let current = List.length history in
        let diff = current - i in
        let (approx, history) = drop diff history in
        List.fold_left (fun i -> fun f -> i + outputs history f) 0 approx.func
 
let rec typecheck af = 
    let rec typecheck' = function 
    | Value _ -> true
    | Input _ -> true
    | Apply (g, f) -> 
        let outputs = outputs af.history f in
        typecheck' f && outputs == gate_inputs g
    | Par fs -> List.for_all typecheck' fs
    | Approx (i, j) -> typecheck (get_past_approximant af i)
    in List.for_all (typecheck') af.func

let rec depends_on ap =
    let rec depends_on' = function
        | Value _       -> []
        | Input (i, _)  -> [i]
        | Apply (g, f)  -> depends_on' f
        | Par fs        -> List.concat (List.map depends_on' fs)
        | Approx (i, j) -> depends_on (get_past_approximant ap i)
    in List.concat (List.map depends_on' ap.func)
    

let rec func_to_string id af =
    let rec func_to_string' = function
    | Value v -> value_to_string v
    | Input (i, j) -> "σ(" ^ (string_of_int i) ^ ")[" ^ (string_of_int j) ^ "]"
    | Apply (g, f) -> (gate_to_string g) ^ "(" ^ (func_to_string' f) ^ ")"
    | Par fs -> list_to_string fs "" "" ", " func_to_string'
    | Approx (i, j) -> id ^ (string_of_int i) ^ "(σ)" ^ "[" ^ (string_of_int j) ^ "]"
    in let tuple = List.fold_left (fun acc -> fun cur -> acc ^ (func_to_string' cur) ^ ", ") "" af.func
    in let tuple = if String.length tuple >= 2 then String.sub tuple 0 (String.length tuple - 2) else tuple
    in "(" ^ tuple ^  ")"

(*

let rec eval_func f xs = match f with
    | Value v -> [v]
    | Input (i, j) -> [List.nth (List.nth xs i) j]
    | Apply (g, f) -> 
        let f = eval_func f xs in 
        [eval_gate g f]
    | Par fs -> 
        List.concat (List.map (fun f -> eval_func f xs) fs)

let eval_func_list fs xs = List.concat (List.map (fun f -> eval_func f xs) fs)

type approximant = int -> func list *)