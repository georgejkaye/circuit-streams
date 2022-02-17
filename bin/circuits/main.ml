open Core.Helpers
open Logic.Values
(* open Circuits.Latch *)
open Circuits.Flipflop
open Circuits.Circuit

let () = 
  (* let latch = sr_latch 0 1 0 0 0 0 in
  let s = 
    make_waveform [
      (High, 5);
      (Low, 10);
      (High, 5);
    ]
  in
  let r = 
    make_waveform [
      (Low, 7);
      (High, 2);
      (Low, 15)
    ]
  in *)
  (* let inputssr = list_of_inputs_to_input_list [r;s] in *)
  let flipflop = d_flipflop 0 0 0 0 0 0 1 1 0 0 0 in
  let d = 
    make_waveform [
      (High, 10);
      (Low, 15);
      (High, 5);
      (Low, 20);
    ]
  in
  let clk = make_clock 5 30
  in
  let inputsff = list_of_inputs_to_input_list [d;clk] in
    print_endline "D flip flop";
    print_endline "n  D Clk Q Q'";
    List.fold_left 
    (fun _ -> fun cur -> 
        let outsff = evaluate_circuit cur inputsff flipflop in
        print_endline (
            (string_of_int cur) ^ (if cur < 10 then " " else "") ^ " " ^
            belnap_value_to_string (List.nth (List.nth inputsff cur) 0) ^ " " ^
            belnap_value_to_string (List.nth (List.nth inputsff cur) 1) ^ "   " ^
            belnap_value_to_string (List.nth outsff 0) ^ " " ^
            belnap_value_to_string (List.nth outsff 1)
        )
    )
    ()
    (nats 40)