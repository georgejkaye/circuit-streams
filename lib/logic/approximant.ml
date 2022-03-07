open Helpers.Lists
open Helpers.Arrays

open Values
open Gates

(* TODO consider if we want to use -1,-2, etc for defining things *)
type approximant_func =
    | Value of belnap_value
    (* For input sigma, Input(i,j) is the jth element of the ith most recent
        stream element, i.e. if this is f(sigma)(i), Input(0, j) = sigma(i)(j),
        Input(1, j) = sigma(i-1)(j) and so on 
    *)
    | Input of int * int
    | Apply of gate * approximant_func array
    (* Approx(i, j) is the jth element of the ith most recent approximant
        i.e. if this is f(sigma)(i), Approx(0, j) is forbidden, Approx(1, j) 
        is f(sigma)(i-1)[j], Approx(2, j) is f(sigma)(i-2)[j] and so on
     *)
    | Approx of int * int

type approximant = {
    func: approximant_func array;
}

let rec func_equality f g = match (f, g) with
    | (Value v, Value w) -> v == w
    | (Input(i, j), Input(k, l)) -> i == k && j == l
    | (Apply(g, f), Apply(h, k)) -> g == h && Array.for_all2 func_equality f k
    | (Approx(i, j), Approx(k, l)) -> i == k && j == l
    | _ -> false

type history = {
    size: int;
    history: approximant list;
}

let drop_history hist i =
    let new_history = drop (i+1) hist.history in
    (fst new_history, { size = hist.size - i ; history = snd new_history})

let get_nth_approximant hist n = List.nth hist.history (hist.size - n - 1)
(* When referencing an approximant of f within another approximant, 
   f0 means the current one, f1 means the one before that, and so on 
   However, not that we shouldn't reference the current approximant since we've 
   already eliminated 'instant feedback' by using the fixpoint operator
*)
let get_nth_most_recent_approximant hist n = List.nth hist.history n

let rec outputs history = function
    | Value _ -> 1
    | Input _ -> 1
    | Apply _ -> 1
    | Approx (i, _) -> 
        let current = List.length history in
        let diff = current - i in
        let (approx, history) = drop diff history in
        Array.fold_left (fun i -> fun f -> i + outputs history f) 0 approx.func
 
let rec typecheck af hist = 
    let typecheck' = function 
    | Value _ -> true
    | Input _ -> true
    | Apply (g, f) -> Array.length f == gate_inputs g
    | Approx (i, _) -> 
        let hist = drop_history hist i in
        typecheck (fst hist) (snd hist) 
    in Array.for_all (typecheck') af.func

let rec depends_on ap hist =
    let rec depends_on' = function
        | Value _       -> []
        | Input (i, _)  -> [i]
        | Apply (_, f)  -> Array.fold_left (fun acc -> fun cur -> acc @ depends_on' cur) [] f
        | Approx (i, _) -> 
            let hist = drop_history hist i in
            depends_on (fst hist) (snd hist)
    in Array.fold_left (fun acc -> fun cur -> acc @ (depends_on' cur)) [] ap.func
    

let func_to_string id n af =
    let rec func_to_string' = function
    | Value v -> belnap_value_to_string v
    | Input (i, j) -> "σ(" ^ (string_of_int (n-i)) ^ ")[" ^ (string_of_int j) ^ "]"
    | Apply (g, f) -> 
        let arguments = array_to_string f "(" ")" ", " func_to_string' in
        (gate_to_string g) ^ arguments
    | Approx (i, j) -> id ^ "(σ)(" ^ (string_of_int (n-i)) ^ ")[" ^ (string_of_int j) ^ "]"
    in array_to_string af.func "(" ")" ", " func_to_string'


let eval_func hist ap xs = 
    let rec eval_func' = function
    | Value v -> v
    | Input (i, j) -> xs.(i).(j)
    | Apply (g, fs) -> 
        let fs = Array.map eval_func' fs in eval_gate g fs
    | Approx (i, j) -> eval_func' (get_nth_most_recent_approximant hist i).func.(j)
    in Array.map eval_func' ap.func

let partial_eval_join x y = match (x, y) with
    | (Value Non, y) -> y
    | (x, Value Non) -> x
    | (Value Both, _) -> Value Both
    | (_, Value Both) -> Value Both
    | (Value High, Value Low) -> Value Both
    | (Value Low, Value High) -> Value Both
    | (Value High, Value High) -> Value High
    | (Value Low, Value Low) -> Value Low
    | (_, _) -> Apply (Join, [| x ; y |])

let partial_eval_or x y = match (x, y) with
    | (Value Non, Value Non) -> Value Non
    | (Value Low, y) -> y
    | (x, Value Low) -> x
    | (Value High, _) -> Value High
    | (_, Value High) -> Value High
    | (Value Both, Value Both) -> Value Both
    | (Value Non, Value Both) -> Value High
    | (Value Both, Value Non) -> Value High
    | (_, _) -> Apply (Or, [| x ; y |])

let partial_eval_and x y = match (x, y) with
    | (Value Non, Value Non) -> Value Non
    | (Value High, y) -> y
    | (x, Value High) -> x
    | (Value Low, _) -> Value Low
    | (_, Value Low) -> Value Low 
    | (Value Both, Value Both) -> Value Both
    | (Value Both, Value Non) -> Value Low
    | (Value Non, Value Both) -> Value Low
    | (_, _) -> Apply (And, [| x ; y |])

let partial_eval_not x = match x with
    | Value Non -> Value Non
    | Value High -> Value Low
    | Value Low -> Value High
    | Value Both -> Value Both
    | _ -> Apply (Not, [| x |])

let partial_eval_gate (g : gate) xs = match g with
| And -> partial_eval_and xs.(0) xs.(1)
| Or -> partial_eval_or xs.(0) xs.(1)
| Not -> partial_eval_not xs.(0)
| Nor -> partial_eval_not (partial_eval_or xs.(0) xs.(1))
| Nand -> partial_eval_not (partial_eval_and xs.(0) xs.(1))
| Join -> partial_eval_join xs.(0) xs.(1)
| AndN _ -> Array.fold_left partial_eval_and (Value High) xs
| OrN _ -> Array.fold_left partial_eval_or (Value Low) xs
| NandN _ -> partial_eval_not (Array.fold_left partial_eval_and (Value High) xs)
| NorN _ -> partial_eval_not (Array.fold_left partial_eval_or (Value Low) xs)

let partial_eval a init n ap =
    let rec partial_eval' = function
    | Value v -> Value v
    | Input (i, j) -> if (n - i) == 0 then Value a.(j) else Input (i, j)
    | Apply (g, fs) -> 
        let fs = Array.map partial_eval' fs in partial_eval_gate g fs
    | Approx (i, j) -> 
        if (n - i) == 0 then Value init.(j) else Approx(i , j)
    in {
        func = Array.map partial_eval' ap.func
    }

let expand hist ap = 
    let rec expand' hist = function
    | Value v -> Value v
    | Input (i, j) -> Input (i, j)
    | Apply (g, f) -> Apply (g, Array.map (expand' hist) f)
    | Approx (i, j) -> 
        let (new_current, new_hist) = drop_history hist i in
        expand' new_hist new_current.func.(j)
    in {
        func = Array.map (expand' hist) ap.func;
    }