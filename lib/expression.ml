open Values
open Helpers

(* Weighting on one side of the classical interpretation *)

type 'a logical_expression =
    | Var of int
    | Constant of 'a
    | Not of 'a logical_expression
    | And of 'a logical_expression list
    | Or of 'a logical_expression list
    | Join of 'a logical_expression list

let rec eval_belnap_logical_expression xs = function
    | Var n -> List.nth xs n
    | Constant v -> v
    | Not exp -> 
        let exp = eval_belnap_logical_expression xs exp in
        not_b exp
    | And exps -> 
        let exps = List.map (eval_belnap_logical_expression xs) exps in
        eval_andn exps
    | Or exps ->
        let exps = List.map (eval_belnap_logical_expression xs) exps in
        eval_orn exps
    | Join exps ->
        let exps = List.map (eval_belnap_logical_expression xs) exps in
        eval_joinn exps

let rec substitute p = function
    | Var 0 -> p
    | Var n -> Var n
    | Constant v -> Constant v
    | Not exp -> Not (substitute p exp)
    | And exps -> And (List.map (substitute p) exps)
    | Or exps -> Or (List.map (substitute p) exps)
    | Join exps -> Join (List.map (substitute p) exps)

let substitute_list p = List.fold_left (fun acc -> fun cur -> substitute acc cur) p

(**
  Get an expression in terms of only None (00) and High (01)
  i.e. the content is on the 'right'

  Returns (t,v) where
    - t is the *translation* from the encoded value to the original 
        value in Belnap logic
    - v is the (classical) value that the original (belnap) value 
        was encoded as
*)
let encode_right_weight = function
    | Non -> (False, Var 0)
    | High -> (True, Var 0)
    | Low -> (True, Not (Var 0))
    | Both -> (True, Join ([Var 0 ; Not (Constant High)]))

let decode_right_weight ev = 
    let (translation, encoding) = ev in
    eval_belnap_logical_expression [encoding] translation

(* Printers *)

let rec logical_expression_to_string to_string = function
    | Var x -> "x" ^ string_of_int x
    | Constant v -> to_string v
    | Not exp -> 
        let exp = logical_expression_to_string to_string exp in
        "¬(" ^ exp ^ ")"
    | And exps -> 
        list_to_string exps "" "" " ∧ " (logical_expression_to_string to_string)
    | Or exps ->
        list_to_string exps "" "" " ∨\n\t" (logical_expression_to_string to_string)
    | Join exps ->
        list_to_string exps "" "" " ⊔ " (logical_expression_to_string to_string)

let belnap_expression_to_string = logical_expression_to_string belnap_value_to_string
let classical_expression_to_string = logical_expression_to_string classical_value_to_string