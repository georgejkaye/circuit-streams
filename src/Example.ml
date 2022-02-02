open Belnap
open Approximant
open Streams
open Helpers
open Mealy

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
    let dervs = stream_to_mealy f in
    (* let dervs = compute_all_stream_derivatives f in
    print_short_stream f;
    print_stream 1 f;
    (* if stream_equality (List.nth dervs 0) f then print_endline "is equal" else print_endline "not equal"; *)
    List.fold_left 
        (fun _ -> fun cur -> print_short_stream cur ; print_stream 1 cur ; 
            if stream_equality cur f then print_endline "equal" else print_endline "not equal"
        )
        () dervs;
     *)
    List.fold_left (fun _ -> fun cur -> print_short_stream cur; print_endline "") () dervs 