open Circuit

let sr_latch a b c d e f =
    let rec f_block = {
        id = 0;
        ports = [(Input 0, a); (Block g_block, b)];
        gate = Nor;
    } and g_block = {
        id = 1;
        ports = [(Block f_block, c) ; (Input 1, d)];
        gate = Nor;
    } 
    in
    {
        input_names = ["R" ; "S"];
        output_names = ["Q" ; "Q'"];
        outputs = [
            (Block f_block, e);
            (Block g_block, f)
        ]
  }


  let d_flipflop a b c d e f g h i j k = 
    let rec nand0 = {
        id = 0;
        gate = Nand;
        ports = [(Input 0, a) ; (Input 1, b)]
    }
    and not0 = {
        id = 1;
        gate = Not;
        ports = [(Input 0, c)]
    }
    and nand1 = {
        id = 2;
        gate = Nand;
        ports = [(Input 1, d) ; (Block not0, e)]
    }
    and nand2 = {
        id = 3;
        gate = Nand;
        ports = [(Block nand0, f) ; (Block nand3, g)]
    }
    and nand3 = {
        id = 4;
        gate = Nand;
        ports = [(Block nand2, h) ; (Block nand1, i)]
    }
    in
    {
        input_names = ["D" ; "Clk"];
        output_names = ["Q" ; "Q'"];
        outputs = [
            (Block nand2, j);
            (Block nand3, k)
        ]
    }

(** https://en.wikipedia.org/wiki/Flip-flop_(electronics)#/media/File:Edge_triggered_D_flip_flop.svg *)
let positive_edge_d_flip_flop a b c d e f g h i j k l m n o = 
    let rec nand0 = {
        id = 1;
        gate = Nand;
        ports = [(Block nand3, a); (Block nand1, b)];
    } and nand1 = {
        id = 2;
        gate = Nand;
        ports = [(Block nand0, c); (Input 0, d)]
    } and nand2 = {
        id = 3;
        gate = NandN 3;
        ports = [(Block nand1, e); (Input 0, f); (Block nand3, g)]
    } and nand3 = {
        id = 4;
        gate = Nand;
        ports = [(Block nand2, h); (Input 1, i)]
    } and nand4 = {
        id = 5;
        gate = Nand;
        ports = [(Block nand1, j) ; (Block nand5, k)]
    } and nand5 = {
        id = 6;
        gate = Nand;
        ports = [(Block nand4, l) ; (Block nand2, m)]
    }
    in
    {
        input_names = ["Clock";"Data"] ;
        output_names = ["Q"; "Q'"] ;
        outputs = [(Block nand4, n) ; (Block nand5, o)]
    }