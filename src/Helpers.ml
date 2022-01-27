let list_to_string xs lbracket rbracket delimiter to_string = 
    let rec list_to_string' = function
    | [] -> ""
    | [x] -> to_string x
    | (x :: xs) -> to_string x ^ delimiter ^ list_to_string' xs 
    in
    lbracket ^ list_to_string' xs ^ rbracket

let print_list xs lbracket rbracket delimiter to_string = print_endline (list_to_string xs lbracket rbracket delimiter to_string)
let print_list_def xs to_string = print_list xs "[" "]" ";" to_string

let rec max_element = function
| [] -> failwith "[max] empty list"
| [x] -> x
| x :: xs -> let max' = max_element xs in if x > max' then x else max'

let nats n = List.init n (fun x -> x)