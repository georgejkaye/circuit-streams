type value = Bottom | True | False | Top

let vtos = function
| Bottom -> "⊥"
| True -> "t"
| False -> "f"
| Top -> "T"

let all_values = [Bottom; True; False; Top]
type input = value list
type output = value list

let rec insert_at a i = function
| [] -> if i == 0 then [a] else failwith "not enough list"
| (x :: xs) -> if i == 0 then a :: x :: xs else x :: insert_at a (i-1) xs

let all_insertions xs =
    let rec all_insertions' vs =    
        let rec all_insertions'' i acc a xs = 
            let ys = insert_at a i xs in 
            let acc = if (List.mem ys acc) then acc else ys :: acc 
            in match i with 
            | 0 -> acc
            | n -> all_insertions'' (i-1) acc a xs 
        in 
        List.fold_left (fun acc -> fun cur -> all_insertions'' (List.length xs) acc cur xs) [] vs
in all_insertions' all_values

let rec concat_no_dups ys = function
| [] -> ys
| (x :: xs) -> if (List.mem x ys) then (concat_no_dups ys xs) else (x :: concat_no_dups xs ys)

let rec possible_inputs = function
    | 0 -> [[]]
    | n -> let recurse = possible_inputs (n-1) in 
           let current = List.map (fun x -> all_insertions x) recurse in
           List.fold_left (fun acc -> fun cur -> concat_no_dups acc cur) [] current

let extend_word w i = 
    let inputs = possible_inputs i in
    List.map (fun ins -> w @ [ins]) inputs

let rec possible_words i = function
| 0 -> [[]]
| n -> let recurse = possible_words i (n-1) in 
        let newwords = (List.map (fun w -> extend_word w i) recurse) in
                                                List.fold_left (fun acc -> fun cur -> concat_no_dups acc cur) [] newwords

let rec copy xs = function
| 0 -> []
| 1 -> [xs]
| n -> xs :: copy xs (n-1)

type mealy = {
    input: int;
    output: int;
    states: int;
    functions: (int -> (value list -> int * value list));
}


type trace = int list * value list list

let compute_trace mm ins = 
    let (finish, path, ins, word) = List.fold_left (fun ( state, path, ins, word ) -> fun inp ->
                        let ( next , outs ) = mm.functions state inp in
                        ( next , next :: path, inp :: ins, outs :: word )
    ) (0, [0], [], []) ins
    in
    (List.rev ins, List.rev path, List.rev word)

let rec print_list del print = function
| [] -> ""
| [a] -> print a
| (x :: xs) -> print x ^ del ^ print_list del print xs

let print_trace t = print_list " " string_of_int t

let print_input i = "[" ^ print_list " " vtos i ^ "]"
let print_word w = print_list " " print_input w
let print_path = function
| (inword, trace, outword) -> ( print_word inword, print_trace trace, print_word outword)

let paths mm i = 
    let ws = possible_words mm.input i in
    List.fold_left (
        fun acc -> 
            fun cur -> 
                let (states, ins, words) = compute_trace mm cur in
                (states, ins, words) :: acc                
     ) [] ws 

let all_paths mm = 
    let rec all_paths' acc current explored word outputs =
        let explored = current :: explored in
        let inputs = possible_inputs mm.input in
        let nexts = (List.map (fun i -> 
            let ( next, output ) = mm.functions current i in
            let word = i :: word in
            let outputs = output :: outputs in
            if List.mem next explored then 
                let path = (word , (next :: explored) , outputs) in
                if List.mem path acc then acc else path :: acc else all_paths' acc next explored word outputs
        ) inputs) in
        List.fold_left (fun acc -> fun cur -> concat_no_dups acc cur) [] nexts
in let paths = all_paths' [] 0 [] [] []
in List.map ( fun (inp, trace, oup) -> (List.rev inp), (List.rev trace), (List.rev oup)) paths


let fs0 = function
| [Bottom] -> (1 , [True; Bottom])
| [Top] -> (3, [True; Bottom])
| [x] -> (2, [True; Bottom])
| xs -> failwith "type error"

let fs1 = function
| [Bottom] -> (1 , [Bottom; Bottom])
| [Top] -> (3, [Bottom; Bottom])
| [x] -> (2, [Bottom; Bottom])
| xs -> failwith "type error"

let fs2 = function
| [Bottom] -> (1 , [Bottom; True])
| [Top] -> (3, [Bottom; True])
| [x] -> (2, [Bottom; True])
| xs -> failwith "type error"

let fs3 = function
| [Bottom] -> (1 , [Bottom; Top])
| [Top] -> (3, [Bottom; Top])
| [x] -> (2, [Bottom; Top])
| xs -> failwith "type error"

let example = {
    input = 1;
    output = 2;
    states = 4;
    functions = (fun s -> match s with | 0 -> fs0 | 1 -> fs1 | 2 -> fs2 | 3 -> fs3 | n -> failwith "type error")
}
(* 
(* let find_paths mm = 
    let rec find_paths' mm cur explored = 
    List.map (fun )  *)

in find_paths' mm 0 [] *)

let rec check_for_dups = function
| [] -> false
| (x :: xs) -> if List.mem x xs then true else check_for_dups xs


open Format
let rec path_list_printer = function
| [] -> ()
| (x :: xs) -> let (a,b,c) = print_path x in print_string a ; print_string "  |  " ; print_string b ; print_string "  |  " ; print_string c ; print_string "\n"; path_list_printer xs