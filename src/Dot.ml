open Mealy
open Belnap
open Helpers

let generate_dot_from_mealy mm = 
    let graph_options = [("rankdir", "LR");("ranksep", "1.5")] in 
    let node_options = [("shape", "circle");("width","0.5")] in
    let edge_options = [("fontsize", "5");("arrowsize", "0.25")] in
    let node_strings = 
        "s0[shape=doublecircle]" ::    
        List.map (fun s -> "s" ^ string_of_int (s+1)) (nats (mm.states -1)) in
    let edge_strings = 
        List.map 
            (fun ((s, m), (n, t)) -> 
                "s" ^ string_of_int s ^ " -> s" ^ string_of_int t ^ "[label=\"  " ^ value_list_to_string m ^ " | " ^ value_list_to_string n ^ "  \"]"
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
       
let write_dot_to_file mm f = 
    let dot = generate_dot_from_mealy mm in
    let oc = open_out f in
        Printf.fprintf oc "%s" dot;
        close_out oc