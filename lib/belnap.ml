open Helpers
open Order

(* Type of values

    We operate in a four value lattice Bot (None) < True, False < Top (Both)
*)
type value =
    | Bot
    | True
    | False
    | Top

let belnap_value_to_string = function
| Bot -> "N"
| True -> "T"
| False -> "F"
| Top -> "B"

let classical_value_to_string = function
| Bot -> "00"
| True -> "01"
| False -> "10"
| Top -> "11"

let lte x y = match (x, y) with
| (Bot, Bot) -> true
| (True, True) -> true
| (False, False) -> true
| (Bot, _) -> false
| (_, Top) -> true
| (_, _) -> false

(* Computing input words *)

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

(* Ordering inputs *)

let value_order = {
    elements = all_inputs;
    order = [
        (Bot, [True;False;Top]);
        (True, [Top]);
        (False, [Top]);
        (Top, [])
    ]
}

let value_list_order n = 
    let all_values = all_inputs_of_length n in
    {
        elements = all_values;
        order = List.map 
        (fun vs -> 
            let less_thans = List.filter (list_lte value_order vs) all_values
            in (vs, less_thans))
        all_values
    }

(* Evaluating gates *)

type gate = 
    | And
    | Or
    | Not
    | Join
    | AndN of int
    | OrN of int

let gate_to_string = function
| And -> "AND"
| Or  -> "OR"
| Not -> "NOT"
| Join -> "⊔"
| AndN n -> "AND_" ^ string_of_int n
| OrN n -> "OR_" ^ string_of_int n

let gate_inputs = function
| And -> 2
| Or -> 2
| Not -> 1
| Join -> 2
| AndN n -> n
| OrN n -> n

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

let eval_andn = List.fold_left eval_and True
let eval_orn = List.fold_left eval_or False

let eval_gate g xs = match g with
| And -> eval_and (List.nth xs 0) (List.nth xs 1)
| Or  -> eval_or (List.nth xs 0) (List.nth xs 1)
| Not -> eval_not (List.nth xs 0)
| Join -> eval_join (List.nth xs 0) (List.nth xs 1)
| AndN _ -> eval_andn xs
| OrN _ -> eval_orn xs

(* Interpretation as classical logic *)

let belnap_to_classical = function
| Bot -> (False, False)
| False -> (True, False)
| True -> (False, True)
| Top -> (True, True)

let belnap_to_classical_list vs = 
    List.rev
        (List.fold_left 
            (fun acc -> fun (x,y) -> y :: x :: acc) 
            [] 
            (List.map belnap_to_classical vs)
        )

(* Weighting on one side of the classical interpretation *)

type logical_expression =
    | Var of int
    | Constant of value
    | Gate of gate * logical_expression list

let rec eval_logical_expression xs = function
    | Var n -> List.nth xs n
    | Constant v -> v
    | Gate (g, vs) ->
        let evaled_vs = List.map (eval_logical_expression xs) vs in
        eval_gate g evaled_vs

(**
  Get an expression in terms of only Bot (00) and True (01)
  i.e. the content is on the 'right'

  Returns (t,v) where
    - t is the *translation* from the encoded value to the original 
        value in Belnap logic
    - v is the (classical) value that the original (belnap) value 
        was encoded as
*)
let encode_right_weight = function
    | Bot -> (Var 0, False)
    | True -> (Var 0, True)
    | False -> (Gate (Not, [Var 0]), True)
    | Top -> (Gate (Join, [Var 0 ; Gate (Not, [Constant True])]), True)

let decode_right_weight ev = 
    let (translation, encoding) = ev in
    eval_logical_expression [encoding] translation

(* Printers *)

let value_list_to_string belnap vs = 
    let to_string = if belnap then belnap_value_to_string else classical_value_to_string in
    list_to_string vs "" "" "" to_string
let value_list_list_to_string belnap vss = list_to_string vss "[" "]" " ; " (value_list_to_string belnap)