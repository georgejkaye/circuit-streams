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
    let sigma = [[Bot] ; [True] ; [Bot] ; [True] ; [Bot] ; [Top]] in
    print_stream f 5;
    print_endline "";
    print_string "Initial output on N: "; 
    print_value_list (initial_output f [Bot]);
    print_string "Stream derivative on N: "; 
    print_stream (stream_derivative f [Bot]) 5;
    print_value_list_list (simulate f sigma)