open Core.Helpers
open Logic.Values
open Circuits.Latch
open Circuits.Flipflop
open Circuits.Circuit

let () = 
  let latch = sr_latch 0 1 0 0 0 0 in
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
  in
  let inputssr = list_of_inputs_to_input_list [r;s] in
  let flipflop = d_flipflop 1 1 1 1 1 1 1 1 1 1 1 in
  let d = 
    make_waveform [
      (High, 7);
      (Low, 5);
      (High, 3);
      (Low, 6)
    ]
  in
  let clk = 
      make_waveform [
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
        (High, 10);
        (Low, 10);
      ]
  in
  let inputsff = list_of_inputs_to_input_list [d;clk] in
  print_endline "SR Latch";
  List.fold_left 
    (fun _ -> fun cur -> 
        let outssr = evaluate_circuit cur inputssr latch in
        print_endline ((string_of_int cur) ^ " " ^ belnap_value_list_to_string outssr)
    )
    ()
    (nats 30);
    print_endline "";
    print_endline "D flip flop";
    List.fold_left 
    (fun _ -> fun cur -> 
        let outsff = evaluate_circuit cur inputsff flipflop in
        print_endline ((if cur < 10 then " " else "") ^ (string_of_int cur) ^ " " ^ belnap_value_list_to_string outsff)
    )
    ()
    (nats 30)