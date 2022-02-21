open Mealy
open Logic.Values
open Helpers.Help

let generate_dot_from_mealy mm assg   = 
    let graph_options = [("rankdir", "LR");("ranksep", "1.5")] in 
    let node_options = [("shape", "circle");("width","0.5")] in
    let edge_options = [("fontsize", "10");("arrowsize", "1")] in
    let get_node_text s = 
        match assg with 
            | None -> "s" ^ string_of_int s
            | Some assg -> belnap_value_list_to_string (List.assoc s assg)
    in
    let node_strings = 
        ((get_node_text 0) ^ "[shape=doublecircle]") ::    
        List.map (fun s -> get_node_text (s+1)) (nats (mm.states -1)) in
    let edge_strings = 
        List.map 
            (fun ((s, m), (n, t)) -> 
                (get_node_text s) ^ 
                " -> " ^
                (get_node_text t) ^
                "[label=\"  " ^
                belnap_value_array_to_string m ^
                " | " ^
                belnap_value_array_to_string n ^
                "  \"]"
            )
      mm.transition_function
    in
    let create_full_string xs = 
        List.fold_left (
            fun acc -> fun cur -> acc ^ "\n\t" ^ cur)
            ("\t" ^ List.hd xs)
            (List.tl xs)
    in 
    let create_graph_options_string xs =
        "\t" ^ 
        List.fold_left 
            (fun acc -> fun (property, value) -> acc ^ "\n\t" ^ property ^ "=" ^ value)
            (fst (List.hd xs) ^ "=" ^ snd (List.hd xs))
            (List.tl xs)
    in
    let create_options_string selector xs = 
        "\t" ^ List.fold_left (
            fun acc -> fun (property, value) -> acc ^ ", " ^ property ^ "=" ^ value
        )
        (selector ^ "[" ^ (fst (List.hd xs)) ^ "=" ^ (snd (List.hd xs)))
        (List.tl xs) ^ "]"
    in
    let graph_options_string = create_graph_options_string graph_options in
    let node_options_string = create_options_string "node" node_options in
    let edge_options_sting = create_options_string "edge" edge_options in
    let complete_node_string = create_full_string node_strings in
    let complete_edge_string = create_full_string edge_strings in
    "digraph G {\n\n" ^
        graph_options_string ^ "\n\n" ^
        node_options_string ^ "\n\n" ^
        complete_node_string ^ "\n\n" ^ 
        edge_options_sting ^ "\n\n" ^
        complete_edge_string ^ "\n}"
       

let write_to_file f s = 
    let oc = open_out f in
        Printf.fprintf oc "%s" s;
        close_out oc

let write_dot_to_file mm f = 
    let dot = generate_dot_from_mealy mm None in
    write_to_file f dot

let write_assigned_mealy_dot_to_file mm ord f = 
    let dot = generate_dot_from_mealy mm (Some ord) in
    write_to_file f dot