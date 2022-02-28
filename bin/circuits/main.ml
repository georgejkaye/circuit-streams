open Circuits.Examples
open Circuits.Circuit
open Logic.Values
open Dot.Circuit_diagram

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
  let r =
    make_waveform [
      (Low, 15);
      (High, 5); 
      (Low, 40)
    ]
  in
  let clk = make_clock 5 50 false in
  let inputs = list_of_inputs_to_input_array [s; clk; r] in
  let (_, instant) = gated_sr_latch_instant 0 Nand Nand in
  (* let (_, delayed) = gated_sr_latch 0 Nand Nand 1 1 1 1 1 1 in *)
  (* let circuit = compare [delayed ; instant] in *)
  circuit_simulation_to_string 50 inputs instant;
  write_circuit_to_file instant "output.dot";