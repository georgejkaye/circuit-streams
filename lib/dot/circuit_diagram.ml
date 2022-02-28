open Circuits.Circuit
open Logic.Values
open Helpers.Help

open Core

type outports = Output of int | Blockport of int * int

let generate_dot_from_circuit c =
    let graph_options = [("rankdir", "LR");("ranksep", "1.5")] in 
    let node_options = [("shape", "record");("width","0.5")] in
    let edge_options = [("fontsize", "10");("arrowsize", "1")] in
    let get_delay_name out = "d_" ^ match out with | Output i -> "o_" ^ string_of_int i | Blockport (i, j) -> "b" ^ string_of_int i ^ "_" ^ string_of_int j in
    let create_delay out d =
        let name = get_delay_name out in 
        (name, name ^ "[shape=circle, width=0.25, fixedsize=true, label=\"" ^ string_of_int d ^ "\"]") 
    in
    let get_block_name b = "b_" ^ string_of_int b.id in
    let create_block b = 
        let inputs = 
            list_to_string 
                (nats (Array.length b.ports)) "{" "}" " | " 
                (fun i -> "<l" ^ (string_of_int i) ^ ">")
        in
        get_block_name b ^ "[shape=record, label=\"{" ^ inputs ^ "| " ^ (gate_to_string b.gate) ^ " |<r>}\"]"
    in
    let rec generate_nodes nodes edges seen frontier = 
            if List.length frontier == 0 
                then 
                    (nodes, edges) 
                else
                    let (nodes, edges, seen, frontier) = 
                        let (p, d, out) = List.hd frontier in
                        let frontier = if List.length frontier > 1 then List.tl frontier else [] in
                        let (delay_name, delay_node) = create_delay out d in
                        let dest = match out with
                            | Output i -> "out" ^ string_of_int i
                            | Blockport (i, j) -> "b_" ^ string_of_int i ^ ":l" ^ string_of_int j
                        in
                        let delay_edge = delay_name ^ ":e -> " ^ dest ^ ":w" in
                        match p with
                            | Block b -> 
                                let block_edge = "b_" ^ string_of_int b.id ^ ":r:e -> " ^ delay_name in
                                let nodes = (delay_node :: nodes) in
                                let edges = (block_edge :: delay_edge :: edges) in
                                if List.mem b.id seen then (nodes, edges, seen, frontier) else
                                    let block_node = create_block b in 
                                    let seen = b.id :: seen in
                                    let next = List.map (fun i -> let (p,d) = b.ports.(i) in (p,d, Blockport(b.id, i))) (nats_of b.ports) in
                                    let nodes = (block_node :: nodes) in
                                    let frontier = next @ frontier in
                                    (nodes, edges, seen, frontier)
                            | Input i -> 
                                let input_edge = "in" ^ string_of_int i ^ ":e -> " ^ delay_name in
                                let nodes = (delay_node :: nodes) in
                                let edges = (input_edge :: delay_edge :: edges) in
                                (nodes, edges, seen, frontier)
                            | Circuit (c, j) -> 
                                let (p,d,_) = c.outputs.(j) in
                                (nodes, edges, seen, (p,d,out) :: frontier)
                            | Value v -> 
                                let value_edge = "v" ^ belnap_value_to_string v ^ " -> " ^ delay_name in
                                let nodes = (delay_node :: nodes) in
                                let edges = (value_edge :: delay_edge :: edges) in
                                (nodes, edges, seen, frontier)
                    in
                    generate_nodes nodes edges seen frontier

    in
    let (nodes, edges) = generate_nodes [] [] [] (Array.to_list (Array.mapi (fun i -> fun (p,d,_) -> (p,d,Output i)) c.outputs)) in
    let generate_interface_node out i str = 
        let name = if out then "out" else "in" in
        name ^ string_of_int i ^ "[shape=cds, fillcolor=gray, style=filled, label=\"" ^ str ^ "\"]" in
    let generate_value_node v = 
        let value = belnap_value_to_string v in "v" ^ value ^ "[label=\"" ^ value ^ "\"]"
    in
    let output_names = get_output_names c in
    let output_nodes = 
        List.rev 
            (List.fold_left (fun acc -> fun i -> (generate_interface_node true i output_names.(i)) :: acc) [] (nats_of output_names))
    in
    let input_nodes = 
        List.rev 
            (List.fold_left (fun acc -> fun i -> (generate_interface_node false i c.input_names.(i)) :: acc) [] (nats_of c.input_names))
    in
    let value_nodes = 
        List.rev
            (List.fold_left (fun acc -> fun v -> (generate_value_node v) :: acc) [] (all_inputs))
    in
    let nodes = value_nodes @ input_nodes @ output_nodes @ nodes in
    let graph_options_string = create_graph_options_string graph_options in
    let node_options_string = create_options_string "node" node_options in
    let edge_options_sting = create_options_string "edge" edge_options in
    let complete_node_string = create_full_string nodes in
    let complete_edge_string = create_full_string edges in
    "digraph G {\n\n" ^
        graph_options_string ^ "\n\n" ^
        node_options_string ^ "\n\n" ^
        complete_node_string ^ "\n\n" ^ 
        edge_options_sting ^ "\n\n" ^
        complete_edge_string ^ "\n}"

let write_circuit_to_file c f = 
    let dot = generate_dot_from_circuit c in
    write_to_file f dot