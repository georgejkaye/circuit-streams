
open Logic.Values
open Logic.Approximant

open Streams.Functions
open Streams.Mealy
open Streams.Dot

(* Example *)

let f0 = {
    func = [ Input (0, 0) ]
}

let fkp1 = {
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
    func = [ Value High ; Value Non ]
}

let g2kp1 = {
    func = [ Value Non ; Input (1, 0) ]
} and g2kp2 = {
    func = [ Value Non ; Value Low ]
}

let g = {
    name = "g";
    inputs = 1;
    outputs = 2;
    prefix_behaviour = [g0];
    period_behvaiour = [g2kp1; g2kp2]
}

let h0 = {
    func = [ Value Non ]
}

let h1 = {
    func = [ Value Non ]
}

let h2 = {
    func = [ Value High ]
}

let h3 = {
    func = [ Value Low ]
}

let h = {
    name = "h";
    inputs = 0;
    outputs = 1;
    prefix_behaviour = [h0;h1];
    period_behvaiour = [h2;h3]
}

let () = 
    let mealy = stream_to_mealy g in
    let ord = assign_state_values mealy in
    write_assigned_mealy_dot_to_file mealy ord "mealy.dot";
