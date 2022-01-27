let list_to_string xs lbracket rbracket delimiter to_string = 
    let rec list_to_string' = function
    | [] -> ""
    | [x] -> to_string x
    | (x :: xs) -> to_string x ^ delimiter ^ list_to_string' xs 
    in
    lbracket ^ list_to_string' xs ^ rbracket