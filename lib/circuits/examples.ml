open Circuit

let sr_latch a b c d e f =
    let rec f_block = {
        id = 0;
        ports = [| (Input 0, a); (Block g_block, b) |];
        gate = Nor;
    } and g_block = {
        id = 1;
        ports = [| (Block f_block, c) ; (Input 1, d) |];
        gate = Nor;
    } 
    in
    {
        input_names = [| "R" ; "S" |];
        output_names = [| "Q" ; "Q'" |];
        outputs = [|
            (Block f_block, e);
            (Block g_block, f)
        |]
    }


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
        output_names = [| "Q" ; "Q'" |];
        outputs = [|
            (Block nand2, j);
            (Block nand3, k)
        |]
    }

(** https://en.wikipedia.org/wiki/Flip-flop_(electronics)#/media/File:Edge_triggered_D_flip_flop.svg *)
let positive_edge_d_flip_flop a b c d e f g h i j k l m n o = 
    let rec nand0 = {
        id = 0;
        gate = Nand;
        ports = [| (Block nand3, a); (Block nand1, b) |];
    } and nand1 = {
        id = 1;
        gate = Nand;
        ports = [| (Block nand0, c); (Input 0, d) |]
    } and nand2 = {
        id = 2;
        gate = NandN 3;
        ports = [| (Block nand1, e); (Input 0, f); (Block nand3, g) |]
    } and nand3 = {
        id = 3;
        gate = Nand;
        ports = [| (Block nand2, h); (Input 1, i) |]
    } and nand4 = {
        id = 4;
        gate = Nand;
        ports = [| (Block nand1, j) ; (Block nand5, k) |]
    } and nand5 = {
        id = 5;
        gate = Nand;
        ports = [| (Block nand4, l) ; (Block nand2, m) |]
    }
    in
    {
        input_names = [| "Clock";"Data" |] ;
        output_names = [| "Q"; "Q'" |] ;
        outputs = [| (Block nand4, n) ; (Block nand5, o) |]
    }

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
        output_names = [| "Q" ; "Q'" |];
        outputs = [| (Block nand4, 0) ; (Block nand5, 0) |]
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
        output_names = [| "Q" ; "Q'" |];
        outputs = [| (Block nand4, 0) ; (Block nand5, 0) |]
    })

let instant_rising_edge_d_flipflop = 
    let (_, circ1) = instant_block_f 0 in
    (* let (id, circ2) = instant_block_f' id in
    let (_, circ3) = instant_block_f id in 
    let circ1 = set_circuit_inputs circ1 [| Circuit (circ2, 1) ; Input 0 |] in
    let circ2 = set_circuit_inputs circ2 [| Circuit (circ1, 1) ; Input 0 ; Input 1 |] in
    let circ3 = set_circuit_inputs circ3 [| Circuit (circ1, 1) ; Circuit (circ2, 0) |] in *)
    {
        input_names = [| "Clk" ; "Data" |];
        output_names = [| "Q" ; "Q'" |];
        outputs = [| (Circuit (circ1, 0), 0) ; (Circuit (circ1, 1) , 0) |]
    }