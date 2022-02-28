open Circuits.Examples
open Circuits.Evaluate
open Circuits.Display
open Logic.Values
open Dot.Circuit_diagram

let () = 
    let s = 
        make_waveform [
            (Low, 30);
            (High, 10);
            (Low, 12);
            (High, 20);
            (Low, 20);
        ]
    in
    (* let r =
        make_waveform [
            (Low, 15);
            (High, 5); 
            (Low, 40)
        ]
    in *)
    let clk = make_clock 5 300 false in
    let inputs = list_of_inputs_to_input_array [clk; s] in
    let id = 0 in
    (* let (id, instant) = gated_sr_latch_instant id Nand Nand in
    let (_, delayed) = gated_sr_latch id Nand Nand 1 1 1 1 1 1 in
    let circuit = compare [("D", delayed) ; ("I", instant)] in *)
    let (_, circuit) = positive_edge_d_flip_flop id 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 in
    (* let (_, circuit) = instant_rising_edge_d_flipflop id in *)
    write_circuit_to_file circuit "output.dot";
    circuit_simulation_to_string 100 inputs circuit;
    draw_outputs circuit 100 inputs