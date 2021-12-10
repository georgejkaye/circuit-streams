let rec check_for_dups = function
| [] -> false
| (x :: xs) -> if List.mem x xs then true else check_for_dups xs

let rec remove_dups = function
| [] -> []
| (x :: xs) -> if List.mem x xs then remove_dups xs else x :: remove_dups xs

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

let possible_inputs i =
    let rec possible_inputs' = function
        | 0 -> [[]]
        | n -> let recurse = possible_inputs' (n-1) in 
            let current = List.map (fun x -> all_insertions x) recurse in
            List.fold_left (fun acc -> fun cur -> concat_no_dups acc cur) [] current in
    List.rev (possible_inputs' i)

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

type path = {
    inputs: value list list;
    states: int list;
    outputs: value list list;
}

type trace = int list * value list list

let compute_trace mm ins = 
    let (finish, path, ins, word) = List.fold_left (fun ( state, path, ins, word ) -> fun inp ->
                        let ( next , outs ) = mm.functions state inp in
                        ( next , next :: path, inp :: ins, outs :: word )
    ) (0, [0], [], []) ins
    in
    {inputs = List.rev ins; states = List.rev path; outputs = List.rev word}

let next_states mm s = 
    List.map (fun v -> let (next, _) = mm.functions s v in next) (possible_inputs mm.input)

let rec print_list del print = function
| [] -> ""
| [a] -> print a
| (x :: xs) -> print x ^ del ^ print_list del print xs

let print_trace t = print_list " " string_of_int t

let print_input i = "[" ^ print_list " " vtos i ^ "]"
let print_word w = print_list " " print_input w
let print_path p = ( print_word p.inputs, print_trace p.states, print_word p.outputs )


let paths mm i = 
    let ws = possible_words mm.input i in
    List.fold_left (
        fun acc -> 
            fun cur -> 
                let trace = compute_trace mm cur in
                trace :: acc                
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
in List.map ( fun (inp, trace, oup) -> {inputs = inp; states = trace; outputs = oup }) paths



(* Assign values to each states 

   We can't use 'pure' one-hot encodings since we need to preserve monotonicity. 
   However (thanks to David Sprunger) there is a way, as long as V is a power of 2.

   Say we have states {s0, s1, s2, s3} and the order

                s3
              /   \
            s1    s2
              \  /
               s0

   Then we can encode these in a way compatible with monotonicity by assigning them
   each a bitstring of 4 bits, where b_k is 1 if s >= s_k and 0 otherwise.
*)

(* let compute_order mm = 
    let ps = all_paths mm in
 *)


let lte x y = match (x, y) with
| (_, Top) -> Some true
| (Top, _) -> Some false
| (Bottom, _) -> Some true
| (_, Bottom) -> Some false
| (_, _) -> None

let path_lte xs ys = if List.length xs != List.length ys then None else 
    let bits = List.map2 lte xs ys in
    if List.mem None bits then None else if List.mem (Some true) bits && List.mem (Some false) bits then None else List.hd bits

type partial_order = int -> int -> bool option

let expo i j = match (i, j) with
| (0, _) -> None
| (_, 0) -> None
| (1, _) -> Some true
| (_, 1) -> Some false
| (2, _) -> Some true
| (_, 2) -> Some false
| (3, 3) -> Some true
| _ -> failwith "not enough states"

let generate_state_values i po = List.map (
    fun s -> List.init i (fun i -> match po i s with | Some b -> if b then True else False | None -> False)
) (List.init i (fun x -> x))

type one_hot_state = value list

type one_hot_mealy = {
    inputs: int;
    outputs: int;
    states: one_hot_state list;
    functions: (int -> (value list -> int * value list));
}

let to_one_hot mm assg = {
    inputs = mm.input;
    outputs = mm.output;
    states = assg;
    functions = mm.functions;
}

(* Examples *)

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
    functions = (fun s -> match s with | 0 -> fs0 | 1 -> fs1 | 2 -> fs2 | 3 -> fs3 | n -> failwith "type error");
}
let omm = to_one_hot example [[False;False;False;True];[False;False;True;False];[False;True;True;False];[True;True;True;False]];;



(* Printers *)

open Format
let print_all_transitions (mm : mealy) =
    let print_transitions_from_state i = 
        let print_transition x = 
            let (state, output) = mm.functions i x in
            print_string ("s" ^ (string_of_int i) ^ " --- " ^ (print_input x) ^ " | " ^ (print_input output) ^ " ---> s" ^ (string_of_int state) ^ "\n")
        in
        List.fold_left (fun acc -> fun cur -> print_transition cur) () (possible_inputs mm.input)
    in
    List.fold_left (fun acc -> fun cur -> print_transitions_from_state cur) () (List.init mm.states (fun i -> i))

let print_onehot_table omm = 
    let print_table_from_state i = 
        let print_row x =
            let (state, output) = omm.functions i x in
            print_string (print_input (List.nth omm.states i) ^ " | " ^ (print_input x) ^ " | " ^ (print_input (List.nth omm.states state)) ^ " | " ^ (print_input output) ^ "\n")
        in
        List.fold_left (fun acc -> fun cur -> print_row cur) () (possible_inputs omm.inputs)
    in
    List.fold_left (fun acc -> fun cur -> print_table_from_state cur) () (List.init (List.length omm.states) (fun i -> i))


let mealy_printer mm = 
    print_string ((string_of_int mm.input) ^ "--" ^ (string_of_int mm.states) ^ "-->" ^ (string_of_int mm.output)) ; print_string "\n" ; print_all_transitions mm

let one_hot_mealy_printer omm = 
    print_string ((string_of_int omm.inputs) ^ "--" ^ (string_of_int omm.outputs)) ; print_string "\n" ; print_onehot_table omm


let rec path_list_printer = function
| [] -> ()
| (x :: xs) -> let (a,b,c) = print_path x in print_string a ; print_string "  |  " ; print_string b ; print_string "  |  " ; print_string c ; print_string "\n"; path_list_printer xs
