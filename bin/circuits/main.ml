open Logic.Values
open Circuits.Examples
open Circuits.Circuit

let () = 
  (* let latch = sr_latch 1 1 1 1 1 1 in *)
  let s = 
    make_waveform [
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
  let clk = make_clock 2 30 in
  let inputs = list_of_inputs_to_input_array [clk;s] in
  print_endline "D flipflop\n";
  circuit_simulation_to_string 50 inputs instant_rising_edge_d_flipflop