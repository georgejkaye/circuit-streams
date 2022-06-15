open Helpers.Lists

open Logic.Values
open Logic.Approximant

open Streams.Functions
open Streams.Mealy
open Streams.Diagram
open Streams.Table
open Streams.Expression

(* Example *)

let f0 = {
    func = [| Input (0, 0) |]
}

let fkp1 = {
    func = [| Apply (Join, [| Approx (1, 0) ; Input (0, 0) |]) |]
}

let f = {
    name = "f";
    inputs = 1;
    outputs = 1;
    prefix_behaviour = [f0];
    period_behvaiour = [fkp1];
}

let g0 = {
    func = [| Value High ; Value Non |]
}

let g2kp1 = {
    func = [| Value Non ; Input (1, 0) |]
} and g2kp2 = {
    func = [| Value Non ; Value Low |]
}

let g = {
    name = "g";
    inputs = 1;
    outputs = 2;
    prefix_behaviour = [g0];
    period_behvaiour = [g2kp1; g2kp2]
}

let h0 = {
    func = [| Value Non |]
}

let h1 = {
    func = [| Value Non |]
}

let h2 = {
    func = [| Value High |]
}

let h3 = {
    func = [| Value Low |]
}

let h = {
    name = "h";
    inputs = 0;
    outputs = 1;
    prefix_behaviour = [h0;h1];
    period_behvaiour = [h2;h3]
}

let () = 
    let mealy = stream_to_mealy g in
    let ord = assign_state_values mealy in
    write_assigned_mealy_dot_to_file mealy ord "mealy.dot";
    let (t,_) = mealy_to_truth_tables mealy (assign_state_values mealy) in
    print_endline (truth_table_to_string belnap_value_list_to_string t);
    let pos = belnap_truth_table_to_positive_classical_table t in
    let neg = belnap_truth_table_to_positive_classical_table t in
    let posexp = convert_classical_table_to_dnf pos in
    let negexp = convert_classical_table_to_dnf neg in
    let inputs = [| Non ; Non ; Non ; Non ; Non ; Both ; High |] in
    let pos_inputs = Array.map belnap_to_positive inputs in
    let neg_inputs = Array.map belnap_to_positive inputs in
    (* print_endline (list_to_string_def elnap_expression_to_string);
    print_endline (list_to_string_def negexp belnap_expression_to_string); *)
    print_endline (list_to_string_def 
        (List.map 
            (fun bit -> eval_positive_logical_expression pos_inputs bit)
            posexp
        )
        classical_value_to_string
    );
    print_endline (list_to_string_def 
        (List.map 
            (fun bit -> eval_negative_logical_expression neg_inputs bit)
            negexp
        )
        classical_value_to_string
    )