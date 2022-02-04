open Helpers
type value =
    | Bot
    | True
    | False
    | Top

let value_to_string = function
| Bot -> "N"
| True -> "T"
| False -> "F"
| Top -> "B"

let all_inputs = [Bot; True; False; Top]

(* TODO this is very slow for n > 4 *)
let all_inputs_of_length n = 
    let rec all_inputs_of_length' = function
        | 0 -> [[]]
        | n -> let rest = all_inputs_of_length' (n-1) in
            List.fold_left (fun acc -> fun v -> 
                    acc @ List.fold_left (fun acc -> fun cur -> acc @ (all_insertions v cur)) [] rest 
            )
            []
            all_inputs
    in remove_duplicates (all_inputs_of_length' n)

let value_list_to_string vs = list_to_string vs "" "" "" value_to_string
let value_list_list_to_string vss = list_to_string vss "[" "]" " ; " value_list_to_string

let print_value v = print_endline (value_to_string v)
let print_value_list vs = print_endline (value_list_to_string vs)
let print_value_list_list vss = print_endline (value_list_list_to_string vss)

let constant_stream v n = List.init n (fun n -> v)

type gate = 
    | And
    | Or
    | Not
    | Join

let gate_to_string = function
| And -> "AND"
| Or  -> "OR"
| Not -> "NOT"
| Join -> "⊔"

let gate_inputs = function
| And -> 2
| Or -> 2
| Not -> 1
| Join -> 2

let eval_and x y = match (x, y) with
| (True, x) -> x
| (x, True) -> x
| (False, _) -> False
| (_, False) -> False
| (Bot, Bot) -> Bot
| (Bot, Top) -> False
| (Top, Bot) -> False
| (Top, Top) -> Top

let eval_or x y = match (x, y) with
| (True, _) -> True
| (_, True) -> True
| (False, x) -> x
| (x, False) -> x
| (Bot, Bot) -> Bot
| (Bot, Top) -> True
| (Top, Bot) -> True
| (Top, Top) -> Top

let eval_not x = match x with
| Bot -> Bot
| True -> False
| False -> True
| Top -> Top

let eval_join x y = match (x, y) with
| (Top, _) -> Top
| (_, Top) -> Top
| (False, True) -> Top
| (True, False) -> Top
| (False, _) -> False
| (_, False) -> False
| (True, _) -> True
| (_, True) -> True
| (Bot, Bot) -> Bot

let eval_gate g xs = match g with
| And -> eval_and (List.nth xs 0) (List.nth xs 1)
| Or  -> eval_or (List.nth xs 0) (List.nth xs 1)
| Not -> eval_not (List.nth xs 0)
| Join -> eval_join (List.nth xs 0) (List.nth xs 1)