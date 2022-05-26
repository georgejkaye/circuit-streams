(* open Circuits.Examples *)
(* open Circuits.Display *)
(* open Circuits.Make *)
open Circuits.Evaluate
(* open Logic.Values *)
(* open Circuits.Diagram *)
open Circuits.Core

let () = 
    (* let s = 
        make_waveform [
            (Low, 30);
            (High, 10);
            (Low, 12);
            (High, 10);
            (Low, 20);
            (High, 10);
            (Low, 30)
        ]
    in
    let r =
        make_waveform [
            (Low, 15);
            (High, 5); 
            (Low, 47);
            (High, 5);
            (Low, 40)
        ]
    in
    let clk = make_clock 5 300 false in
    let inputs = transpose_inputs [s ; clk ; r] in
    let id = 0 in
    let (id, instant) = gated_sr_latch_instant id Nand Nand in
    let (_, delayed) = gated_sr_latch id Nand Nand 1 1 1 1 1 1 in
    let circuit = compare [("D", delayed) ; ("I", instant)] in
    (* let (_, circuit) = positive_edge_d_flip_flop id 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 in *)
    (* let (_, circuit) = instant_rising_edge_d_flipflop id in *)
    write_circuit_to_file circuit "output.dot";
    string_of_simulation 100 inputs circuit;
    print_waveforms circuit 100 inputs *)
    let c = 
        let b1 = {
            id = 0;
            ports = [|
                (Input 0, 0) ;
                (Input 1, 0)
            |];
            gate = And;
        }
        in let b2 = {
            id = 1;
            ports = [|
                (Input 0, 0);
                (Input 1, 0)
            |];
            gate = Or;
        } 
        in
        {
        input_names = [| "x" ; "y" |];
        outputs = [|
            (Block b1, 0, "s") ;
            (Block b2, 0, "t") 
        |]
        }
        in
        let d = 
            let b1 = {
                id = 0;
                ports = [|
                    (Input 0, 0) ;
                    (Input 1, 0)
                |];
                gate = And;
            }
            in let b2 = {
                id = 1;
                ports = [|
                    (Input 0, 0);
                    (Input 1, 0)
                |];
                gate = Or;
            } 
            in let b3 = {
                id = 2;
                ports = [|
                    (Block b1, 0);
                    (Block b1, 0)
                |];
                gate = Or;
            }
            in
            {
            input_names = [| "x" ; "y" |];
            outputs = [|
                (Block b3, 0, "s") ;
                (Block b2, 0, "t") 
            |]
            }
    in 
    if equal_combinational c d then print_endline "same" else print_endline "not same";