open Belnap
open Approximant
open Streams
open Helpers

(* Example *)

let f0 = {
    inputs = 1;
    outputs = 1;
    current = 0;
    history = [];
    func = [ Input (0, 0) ]
}

let rec fkp1 i = {
    inputs = 1;
    outputs = 1;
    current = i + 1;
    history = List.map (fun i -> if i == 0 then f0 else fkp1 (i-1)) (revnats i);
    func = [ Apply (Join, [Approx (i, 0) ; Input (i+1, 0)])]
}

let f = {
    behaviour = fun i ->  match i with | 0 -> f0 | k -> fkp1 (i-1)
}

let fex = {
    behaviour = fun i ->  match i with | 0 -> expand f0 | k -> expand (fkp1 (i-1))
}

(* let f0 i = 
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

let f : circuit_stream = (1, 1, [f0], [f1;f2]) *)

let () = 
    let sigma = [[Bot] ; [True] ; [Bot] ; [True] ; [False] ; [Bot]] in
    print_value_list_list (simulate f sigma);
    print_endline "";
    print_endline (func_to_string "f" (f.behaviour 0));
    print_endline (func_to_string "f" (f.behaviour 1));
    print_endline (func_to_string "f" (f.behaviour 2));
    print_endline (func_to_string "f" (fex.behaviour 2))
    (* let sigma = [Top] :: (constant_stream [True] 10) in () *)
    (* let evaled = eval f 6 sigma in
    print_value_list_list evaled *)