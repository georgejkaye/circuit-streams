open Circuit_streams.Values
open Circuit_streams.Approximant
open Circuit_streams.Streams
open Circuit_streams.Mealy
open Circuit_streams.Dot
open Circuit_streams.Function
open Circuit_streams.Helpers
open Circuit_streams.Expression

(* Example *)

let f0 = {
    func = [ Input (0, 0) ]
}

let fkp1 = {
    func = [ Apply (Join, [Approx (1, 0) ; Input (0, 0)])]
}

let f = {
    name = "f";
    inputs = 1;
    outputs = 1;
    prefix_behaviour = [f0];
    period_behvaiour = [fkp1];
}

let g0 = {
    func = [ Value High ; Value Non ]
}

let g2kp1 = {
    func = [ Value Non ; Input (1, 0) ]
} and g2kp2 = {
    func = [ Value Non ; Value Low ]
}

let g = {
    name = "g";
    inputs = 1;
    outputs = 2;
    prefix_behaviour = [g0];
    period_behvaiour = [g2kp1; g2kp2]
}

let h0 = {
    func = [ Value Non ]
}

let h1 = {
    func = [ Value Non ]
}

let h2 = {
    func = [ Value High ]
}

let h3 = {
    func = [ Value Low ]
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
    let (output,_) = mealy_to_truth_tables mealy ord in
    print_endline (belnap_truth_table_to_string output);
    let new_table = right_weight_truth_table output in
    print_endline (classical_truth_table_to_string new_table);
    (* print_endline (
    (list_to_string 
        (Array.to_list translation.translations) "" "" "\n" 
        (fun row -> 
            list_to_string (Array.to_list (fst row)) "" "" " __ " belnap_expression_to_string 
            ^
            " | "
            ^
            list_to_string (Array.to_list (snd row)) "" "" " __ " belnap_expression_to_string 
        )
    )
    ); *)
    let dnfs = convert_classical_table_to_dnf new_table in
    print_endline (list_to_string dnfs "" "" "\n" belnap_expression_to_string);
    print_endline "";
    (* let decoded_dnfs = decode_right_weighted_dnfs dnfs translation in
    print_endline (list_to_string decoded_dnfs "" "" "\n" belnap_expression_to_string);
    let check_inputs = [Both;Non;Non;Non;Non;Non;High] in
    let check = List.map (eval_belnap_logical_expression check_inputs) decoded_dnfs in
    print_endline (belnap_value_list_to_string check) *)
    
    (* print_endline "\n";
    print_endline (truth_table_to_string false transition) *)
