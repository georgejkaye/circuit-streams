
let rec drop n = function
    | [] -> failwith "[drop] Not enough list"
    | (x :: xs) -> 
        if n == 0 then failwith "[drop] Can't drop nothing"
        else if n == 1 then (x, xs) else drop (n-1) xs

let list_to_string xs lbracket rbracket delimiter to_string = 
    let rec list_to_string' = function
    | [] -> ""
    | [x] -> to_string x
    | (x :: xs) -> to_string x ^ delimiter ^ list_to_string' xs 
    in
    lbracket ^ list_to_string' xs ^ rbracket

let list_to_string_def xs to_string = list_to_string xs "[" "]" ";" to_string

let print_list xs lbracket rbracket delimiter to_string = print_endline (list_to_string xs lbracket rbracket delimiter to_string)
let print_list_def xs to_string = print_list xs "[" "]" ";" to_string

let rec max_element = function
| [] -> failwith "[max] empty list"
| [x] -> x
| x :: xs -> let max' = max_element xs in if x > max' then x else max'

let nats n = List.init n (fun x -> x)

let rec revnats n = match n with
| 0 -> [0]
| n -> n :: revnats (n-1)