open Helpers
type value =
    | Bot
    | True
    | False
    | Top

let value_to_string = function
| Bot -> "⊥"
| True -> "t"
| False -> "f"
| Top -> "⊤"

let all_inputs = [Bot; True; False; Top]

let value_list_to_string vs = list_to_string vs "" "" " " value_to_string
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