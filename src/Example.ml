open Belnap
open Approximant
open Streams
open Helpers

(* Example *)

let f0 = {
    func = [ Input (0, 0) ]
}

let rec fkp1 = {
    func = [ Apply (Join, [Approx (1, 0) ; Input (0, 0)])]
}

let f = {
    name = "f";
    inputs = 1;
    outputs = 1;
    prefix_behaviour = [f0];
    period_behvaiour = [fkp1];
}

let g0 = {
    func = [ Value True ; Value Bot ]
}

let rec g2kp1 = {
    func = [ Value Bot ; Input (1, 0) ]
} and g2kp2 = {
    func = [ Value Bot ; Value False ]
}

let g = {
    name = "g";
    inputs = 1;
    outputs = 2;
    prefix_behaviour = [g0];
    period_behvaiour = [g2kp1; g2kp2]
}

let () = 
    let sigma = [[Bot] ; [True] ; [Top] ; [True] ; [False] ; [Bot]] in
    print_value_list_list (simulate g sigma);
    print_endline "";
    print_stream f 6;
    print_stream g 6;
    (* print_endline (func_to_string "f" (f.behaviour 0));
    print_endline (func_to_string "f" (f.behaviour 1));
    print_endline (func_to_string "f" (f.behaviour 2));
    print_endline (func_to_string "f" (fex.behaviour 2)) *)
    (* let sigma = [Top] :: (constant_stream [True] 10) in () *)
    (* let evaled = eval f 6 sigma in
    print_value_list_list evaled *)