open Logic.Values
open Helpers.Strings
open Helpers.Nats

open Core
open Evaluate
open Aux

let high_to_low_u = "┐"
let high_to_low_d = "└"
let low_to_high_u = "┌"
let low_to_high_d = "┘"
let stable = "─"

let none = "."

let short_u = "*"
let short_d = "*"

let string_of_waveforms c n inputs =
    let name_length = 1 + max (get_longest_string (c.input_names)) (get_longest_string (get_output_names c)) in
    let input_names = Array.map (pad_back name_length) c.input_names in
    let output_names = Array.map (pad_back name_length) (get_output_names c) in
    let outputs = simulate_circuit n inputs c in
    let draw_waveforms names vss = 
        List.rev (List.fold_left  
        (fun acc -> fun i -> 
            let (_, str_u, str_d) = List.fold_left 
                (fun (prev, str_u, str_d) -> fun j ->
                    let value = vss.(j).(i) in
                    let (next_u, next_d) = match value with
                        | Non -> (none, none)
                        | High -> (match prev with
                            | Low -> (low_to_high_u, low_to_high_d)
                            | _ -> (stable, none))
                        | Low -> (match prev with 
                            | High -> (high_to_low_u, high_to_low_d)
                            | _ -> (none, stable))
                        | Both -> (short_u, short_d)
                    in
                    (value, str_u ^ next_u, str_d ^ next_d)
                )
                (Non, String.init name_length (fun _ -> ' '), names.(i))
                (nats n)
            in
            ("" :: str_d :: str_u :: acc)
        )
        []
        (nats_of names))
    in
    let input_waveforms = draw_waveforms input_names inputs in
    let output_waveforms = draw_waveforms output_names outputs in
    List.fold_left (fun acc -> fun cur -> acc ^ "\n" ^ cur) "" (input_waveforms @ output_waveforms)

let print_waveforms c n inputs = print_endline (string_of_waveforms c n inputs)