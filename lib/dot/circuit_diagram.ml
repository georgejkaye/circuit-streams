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
        (name, name ^ "[shape=circle, label=\"" ^ string_of_int d ^ "\"]") 
    in
    let get_block_name b = "b" ^ string_of_int b.id in
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
                    let (p, d, out) = List.hd frontier in
                    let (delay_name, delay) = create_delay out d in
                    let dest = match out with
                        | Output i -> "o_" ^ string_of_int i
                        | Blockport (i, j) -> "b_" ^ string_of_int i ^ ":" ^ string_of_int j
                    in
                    let delay_edge = delay_name ^ ":e -> " ^ dest ^ ":w" in
                    let edges = (delay_edge :: edges) in
                    match p with
                        | Block b -> 
                            let block_edge = "b_" ^ string_of_int b.id ^ ":r -> " ^ delay_name in
                            let edges = (block_edge :: edges) in
                            if List.mem b.id seen then (nodes, edges) else
                                let block_node = create_block b in 
                                let nodes = (block_node :: nodes) in
                                let seen = b.id :: seen in
                                let next = List.map (fun i -> let (p,d) = b.ports.(i) in (p,d, Blockport(b.id, i))) (nats (Array.length b.ports)) in
                                generate_nodes (delay :: block_node :: nodes) (delay_edge :: block_edge :: edges) seen (next @ frontier)
                        | Input i -> 
                            let input_edge = "in" ^ string_of_int i ^ ":e -> " ^ delay_name in
                            let edges = (input_edge :: edges) in
                            generate_nodes nodes edges seen frontier
                        | Circuit (c, j) -> 
                            let (p,d) = c.outputs.(j) in
                            generate_nodes nodes edges seen ((p,d,out) :: frontier)
                        | Value v -> 
                            let value_edge = "v" ^ belnap_value_to_string v ^ " -> " ^ delay_name in
                            let edges = (value_edge :: edges) in
                            generate_nodes nodes edges seen frontier
    in
    let (nodes, edges) = generate_nodes [] [] [] (Array.to_list (Array.mapi (fun i -> fun (p,d) -> (p,d,Output i)) c.outputs)) in
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