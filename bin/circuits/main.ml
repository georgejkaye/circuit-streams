open Core.Helpers
open Logic.Values
open Circuits.Latch
open Circuits.Circuit

let () = 
  let latch = sr_latch 1 2 3 4 5 6 in
  let s = [High; High; High; High ; High ; High ; Low ; Low ; Low] in
  let r = [Low; Low; Low; Low; Low; Low; Low; Low] in
  let inputs = list_of_inputs_to_input_list [s;r] in
  List.fold_left 
    (fun _ -> fun cur -> 
        let outs = evaluate_circuit cur inputs latch in
        print_endline (belnap_value_list_to_string outs)
    )
    ()
    (nats 15)