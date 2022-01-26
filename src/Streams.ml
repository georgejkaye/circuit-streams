open Belnap

type func = 
    | Value of value
    (* Arg(i,j) is the jth element of the input at tick i *)
    | Arg of int * int          
    | Apply of gate * func
    | Par of func list

let rec eval_func f xs = match f with
    | Value v -> [v]
    | Arg (i, j) -> [List.nth (List.nth xs i) j]
    | Apply (g, f) -> 
        let f = eval_func f xs in 
        [eval_gate g f]
    | Par fs -> 
        List.concat (List.map (fun f -> eval_func f xs) fs)


(**  
    Causal stream functions are defined for each tick i by using
    approximants f_i : M^i -> N.

    In the case of circuit stream functions, there is a finite
    prefix of approximants followed by a finite recurring period
    of approximants.

    Therefore we specify them as (prefix, period)
*)
type circuit_stream = func list * func list

(* Evaluate a circuit stream up to a given tick *)
let eval f k s = 
    let (prefix, period) = f
    in 0

(* Example *)

let f0 i = 
    [
        Value True ;
        Value Bot
    ]

let f1 i = 
    [
        Value Bot ;
        Arg (i - 1, 0)
    ]

let f2 i = 
    [
        Value Bot ;
        Value False
    ]

let f = ([f0], [f1;f2])