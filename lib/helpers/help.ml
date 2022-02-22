let rec drop n = function
    | [] -> failwith "[drop] Not enough list"
    | (x :: xs) -> 
        if n == 0 then failwith "[drop] Can't drop nothing"
        else if n == 1 then (x, xs) else drop (n-1) xs

let remove a xs = 
    List.rev 
        (List.fold_left 
            (fun acc -> fun cur -> if cur == a then acc else cur :: acc)
            []
            xs
        )

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

let get_max_length vss = 
    List.fold_left (fun acc -> fun cur -> max acc (List.length cur)) 0 vss

let rec max_element = function
| [] -> failwith "[max] empty list"
| [x] -> x
| x :: xs -> let max' = max_element xs in if x > max' then x else max'

let nats n = List.init n (fun x -> x)
let nats_from init n = List.init (n-1) (fun x -> x + init)

let rec revnats n = match n with
| 0 -> [0]
| n -> n :: revnats (n-1)

let insert_at a i xs = 
    let rec insert_at' acc a i = function
    | [] -> if i == 0 then (acc, [a]) else failwith "[insert_at] Not enough list"
    | (x :: xs) -> if i == 0 then (acc, a :: x :: xs) else insert_at' (x :: acc) a (i-1) xs  
    in 
    let (acc, tail) = insert_at' [] a i xs
    in (List.rev acc) @ tail 

let all_insertions a xs = 
    List.map (fun i -> insert_at a i xs) (nats (List.length xs + 1))    

let remove_duplicates xs = 
    List.rev (
        List.fold_left 
            (fun acc -> fun cur -> if List.mem cur acc then acc else cur :: acc)
            []
            xs
    )

let id x = x

let array_to_string xs lbracket rbracket delimiter to_string = 
    let len = Array.length xs in
    let content = 
        if len == 0 
            then 
                "" 
            else
                let first = to_string xs.(0)
                in if len == 1 
                    then 
                        first 
                    else 
                        List.fold_left 
                            (fun acc -> fun i -> acc ^ delimiter ^ to_string xs.(i)) 
                            first 
                            (nats_from 1 len)
    in
    lbracket ^ content ^ rbracket

let array_nats n = Array.init n id

let nats_of xs = nats (Array.length xs)