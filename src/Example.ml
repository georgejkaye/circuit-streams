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
    current = i;
    history = List.map (fun i -> if i == 0 then f0 else fkp1 i) (revnats (i-1));
    func = [ Apply (Join, Par [Approx ((i-1), 0) ; Input (i, 0)])]
}

let f = {
    behaviour = fun i ->  match i with | 0 -> f0 | k -> fkp1 i
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
    print_list_def (depends_on (f0)) string_of_int ;
    print_endline (func_to_string "f" (f0));
    print_endline (func_to_string "f" (fkp1 1));
    print_endline (func_to_string "f" (fkp1 2));
    print_endline (func_to_string "f" (fkp1 3));
    print_endline (func_to_string "f" (fkp1 4))
    (* let sigma = [Top] :: (constant_stream [True] 10) in () *)
    (* let evaled = eval f 6 sigma in
    print_value_list_list evaled *)