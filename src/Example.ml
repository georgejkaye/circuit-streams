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
    print_value_list (eval_func f0 sigma);
    print_value_list (eval_func (fkp1 0) sigma);
    print_value_list (eval_func (fkp1 1) sigma);
    print_value_list (eval_func (fkp1 2) sigma);
    print_value_list (eval_func (fkp1 3) sigma);
    print_value_list (eval_func (fkp1 4) sigma);
    print_endline "";
    print_value_list_list (simulate f sigma)
    (* let sigma = [Top] :: (constant_stream [True] 10) in () *)
    (* let evaled = eval f 6 sigma in
    print_value_list_list evaled *)