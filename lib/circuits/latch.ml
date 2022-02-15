open Circuit

let sr_blocks a b c d = 
  let rec f_block = {
    inputs = [(Input 0, a); (Block g_block, b)];
    gate = Nor;
  } and g_block = {
    inputs = [(Block f_block, c) ; (Input 1, d)];
    gate = Nor;
  } in
  (f_block, g_block)

let sr_latch a b c d e f =
  let (f_block, g_block) = sr_blocks a b c d in
  {
    outputs = [
        (f_block, e);
        (g_block, f)
    ]
  }