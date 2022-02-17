open Circuit

let d_flipflop a b c d e f g h i j k = 
    let rec nand0 = {
        gate = Nand;
        ports = [(Input 0, a) ; (Input 1, b)]
    }
    and not0      = {
        gate = Not;
        ports = [(Input 0, c)]
    }
    and nand1     = {
        gate = Nand;
        ports = [(Input 1, d) ; (Block not0, e)]
    }
    and nand2     = {
        gate = Nand;
        ports = [(Block nand0, f) ; (Block nand3, g)]
    }
    and nand3     = {
        gate = Nand;
        ports = [(Block nand2, h) ; (Block nand1, i)]
    }
    in
    {
        outputs = [
            (nand2, j);
            (nand3, k)
        ]
    }