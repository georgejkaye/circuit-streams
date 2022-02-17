open Logic.Values
open Circuits.Examples
open Circuits.Circuit

let () = 
  let flipflop = positive_edge_d_flip_flop 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 in
  let d = 
    make_waveform [
      (High, 10);
      (Low, 12);
      (High, 5);
      (Low, 20);
    ]
  in
  let clk = make_clock 2 30 in
  let inputs = list_of_inputs_to_input_list [clk;d] in
  print_endline "D flipflop\n";
  circuit_simulation_to_string 50 inputs flipflop