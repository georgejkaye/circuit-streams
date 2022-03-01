open Logic.Values
open Logic.Gates

open Core
open Aux
open Make

let sr_latch id gate a b c d e f =
    let (input_a, input_b) = 
        match gate with
            | Nor -> ("R", "S")
            | Nand -> ("S'", "R'")
            | _ -> failwith ("[sr_latch] Gate incompatible with construction " ^ gate_to_string gate)
    in
    let rec f_block = {
        id = id;
        ports = [| (Input 0, a); (Block g_block, b) |];
        gate = gate;
    } and g_block = {
        id = id + 1;
        ports = [| (Block f_block, c) ; (Input 1, d) |];
        gate = gate;
    } 
    in
    (id + 2, {
        input_names = [| input_a ; input_b |];
        outputs = [|
            (Block f_block, e, "Q");
            (Block g_block, f, "Q'")
        |]
    })

let sr_latch_instant id gate =
    let (input_a, input_b) = match gate with
        | Nor -> ("R", "S")
        | Nand -> ("S", "R")
        | _ -> failwith "Not a valid sr latch gate"
    in
    let make_iteration id = 
        let rec f_block = {
            id = id;
            ports = [| (Input 1, 0) ; (Input 0, 0) |];
            gate = gate;
        } and g_block = {
            id = id + 1;
            ports = [| (Block f_block, 0) ; (Input 2, 0) |];
            gate = gate;
        }
        in
        (id + 2, {
            input_names = [| "Fb" ; input_a ; input_b |];
            outputs = [|
                (Block g_block, 0, "Fb");
                (Block f_block, 0, "Q");
                (Block g_block, 0, "Q'");
            |]
        })
    in
    iterate id (make_iteration) 1 2 2 [| input_a ; input_b |] [| "Q" ; "Q'" |] 

let create_latch_gate id gate =
    let (input_a, input_c) =
        match gate with 
            | Nand -> ("S", "R")
            | And -> ("R", "S")
            | _ -> failwith ("[create_latch_gate] Incompatible gate " ^ gate_to_string gate)
    in
    let top_block = {
        id = id;
        ports = [| (Input 0, 0) ; (Input 1, 0) |];
        gate = gate;
    } in
    let bot_block = {
        id = id + 1;
        ports = [| (Input 1, 0) ; (Input 2, 0) |];
        gate = gate;
    } in
    (id + 2, {
        input_names = [| input_a  ; "E" ; input_c |];
        outputs = [| (Block top_block, 0, "Q") ; (Block bot_block, 0, "Q'") |];
    })


let gated_sr_latch_spec gate latch = 
    let output_names = get_output_names latch in
    combine 
    [|
        (gate, [| Input 0 ; Input 1 ; Input 2 |]);
        (latch, [| Circuit (gate, 0) ; Circuit (gate, 1) |])   
    |]
    [| (1, 0, 0, output_names.(0)) ; (1, 1, 0, output_names.(1)) |]
    gate.input_names

let check_gated_sr_latch_gates gate_gate latch_gate = 
    match (gate_gate, latch_gate) with 
        | (Nand, Nand) -> ()
        | (And, Nor) -> ()
        | _ -> failwith ("[gated_sr_latch] Incompatible gates with construction " ^ gate_to_string gate_gate ^ " " ^ gate_to_string latch_gate)

let gated_sr_latch id gate_gate latch_gate a b c d e f =
    check_gated_sr_latch_gates gate_gate latch_gate;
    let (id, gate) = create_latch_gate id gate_gate in
    let (id, sr_latch) = sr_latch id latch_gate a b c d e f in
    let circuit = gated_sr_latch_spec gate sr_latch in
    print_int (gates circuit);
    (id, circuit)


let gated_sr_latch_instant id gate_gate latch_gate = 
    check_gated_sr_latch_gates gate_gate latch_gate;
    let (id, gate) = create_latch_gate id gate_gate in
    let (id, sr_latch) = sr_latch_instant id latch_gate in
    (id, gated_sr_latch_spec gate sr_latch)
    

let d_flipflop a b c d e f g h i j k = 
    let rec nand0 = {
        id = 0;
        gate = Nand;
        ports = [| (Input 0, a) ; (Input 1, b) |]
    }
    and not0 = {
        id = 1;
        gate = Not;
        ports = [| (Input 0, c) |]
    }
    and nand1 = {
        id = 2;
        gate = Nand;
        ports = [| (Input 1, d) ; (Block not0, e) |]
    }
    and nand2 = {
        id = 3;
        gate = Nand;
        ports = [| (Block nand0, f) ; (Block nand3, g) |]
    }
    and nand3 = {
        id = 4;
        gate = Nand;
        ports = [| (Block nand2, h) ; (Block nand1, i) |]
    }
    in
    {
        input_names = [| "D" ; "Clk" |];
        outputs = [|
            (Block nand2, j, "Q");
            (Block nand3, k, "Q'")
        |]
    }

(** https://en.wikipedia.org/wiki/Flip-flop_(electronics)#/media/File:Edge_triggered_D_flip_flop.svg *)
let positive_edge_d_flip_flop id a b c d e f g h i j k l m n o = 
    let rec nand0 = {
        id = id;
        gate = Nand;
        ports = [| (Block nand3, a); (Block nand1, b) |];
    } and nand1 = {
        id = id+1;
        gate = Nand;
        ports = [| (Block nand0, c); (Input 0, d) |]
    } and nand2 = {
        id = id+2;
        gate = NandN 3;
        ports = [| (Block nand1, e); (Input 0, f); (Block nand3, g) |]
    } and nand3 = {
        id = id+3;
        gate = Nand;
        ports = [| (Block nand2, h); (Input 1, i) |]
    } and nand4 = {
        id = id+4;
        gate = Nand;
        ports = [| (Block nand1, j) ; (Block nand5, k) |]
    } and nand5 = {
        id = id+5;
        gate = Nand;
        ports = [| (Block nand4, l) ; (Block nand2, m) |]
    }
    in
    (id + 6, {
        input_names = [| "Clock"; "Data" |] ;
        outputs = [| (Block nand4, n, "Q") ; (Block nand5, o, "Q'") |]
    })

let instant_block_f i =
    let rec nand0 = {
        id = i;
        gate = Nand;
        ports = [| (Input 0, 0); (Value Non, 0) |]
    } and nand1 = {
        id = i + 1;
        gate = Nand;
        ports = [| (Block nand0, 0) ; (Input 1, 0) |]
    } and nand2 = {
        id = i + 2;
        gate = Nand;
        ports = [| (Input 0, 0) ; (Block nand1, 0) |]
    } and nand3 = {
        id = i + 3;
        gate = Nand;
        ports = [| (Block nand2, 0) ; (Input 1, 0) |]
    } and nand4 = {
        id = i + 4;
        gate = Nand;
        ports = [| (Input 0, 0) ; (Block nand3, 0) |]
    } and nand5 = {
        id = i + 5;
        gate = Nand;
        ports = [| (Block nand4, 0) ; (Input 1, 0) |]
    }
    in 
    (i + 6, {
        input_names = [| ""; "" |];
        outputs = [| (Block nand4, 0, "Q") ; (Block nand5, 0, "Q'") |]
    })

let instant_block_f' i = 
    let rec nand0 = {
        id = i;
        gate = NandN 3;
        ports = [| (Input 0, 0) ; (Input 1, 0) ; (Value Non, 0) |]
    } and nand1 = {
        id = i + 1;
        gate = Nand;
        ports = [| (Block nand0, 0) ; (Input 2, 0) |]
    } and nand2 = {
        id = i + 2;
        gate = NandN 3;
        ports = [| (Input 0, 0) ; (Input 1, 0) ; (Block nand1, 0) |]
    } and nand3 = {
        id = i + 3;
        gate = Nand;
        ports = [| (Block nand2, 0) ; (Input 2, 0) |]
    } and nand4 = {
        id = i + 4;
        gate = NandN 3;
        ports = [| (Input 0, 0) ; (Input 1, 0) ; (Block nand3, 0) |]
    } and nand5 = {
        id = i + 5;
        gate = Nand;
        ports = [| (Block nand4, 0) ; (Input 2, 0) |]
    }
    in
    (i + 6, {
        input_names = [| "" ; "" ; "" |];
        outputs = [| (Block nand4, 0, "Q") ; (Block nand5, 0, "Q'") |]
    })

let combined_instant_blocks id =
    let (id, circ1) = instant_block_f id in
    let (id, circ2) = instant_block_f' id in
    let (id, circ3) = instant_block_f id in 
    let circ = combine 
        [|
            (circ1, [| Input 0 ; Input 1 |]) ;
            (circ2, [| Circuit (circ1, 1) ; Input 1 ; Input 2 |]) ;
            (circ3, [| Circuit (circ1, 1) ; Circuit (circ2, 0) |])
        |]
        [| (1, 1, 0, "Fb") ; (2, 0, 0, "Q") ; (2, 1, 0, "Q'") |]
        [| "Fb" ; "Clk" ; "Data" |]
    in
    (id, circ) 

type outports = Output of int | Blockport of int * int

let instant_rising_edge_d_flipflop id = 
    let (id, combined1) = combined_instant_blocks id in
    let (id, combined2) = combined_instant_blocks id in
    let (id, combined3) = combined_instant_blocks id in
    let circ = combine
        [| 
            (combined1, [| Value Non ; Input 0 ; Input 1 |]) ;
            (combined2, [| Circuit (combined1, 0) ; Input 0 ; Input 1 |]) ;
            (combined3, [| Circuit (combined2, 0) ; Input 0 ; Input 1 |]) ;
        |]
        [| (2, 1, 0, "Q") ; (2, 2, 0, "Q'") |]
        [| "Clk" ; "Data" |]
    in
    (id, circ)