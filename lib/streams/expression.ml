open Logic.Values
open Helpers.Help

(* Weighting on one side of the classical interpretation *)

type 'a logical_expression =
    | Var of int
    | Constant of 'a
    | Not of 'a logical_expression
    | And of 'a logical_expression array
    | Or of 'a logical_expression array
    | Join of 'a logical_expression array

let rec eval_belnap_logical_expression xs = function
    | Var n -> xs.(n)
    | Constant v -> v
    | Not exp -> 
        let exp = eval_belnap_logical_expression xs exp in
        not_b exp
    | And exps -> 
        let exps = Array.map (eval_belnap_logical_expression xs) exps in
        andn_b exps
    | Or exps ->
        let exps = Array.map (eval_belnap_logical_expression xs) exps in
        orn_b exps
    | Join exps ->
        let exps = Array.map (eval_belnap_logical_expression xs) exps in
        eval_joinn exps

let rec substitute p = function
    | Var 0 -> p
    | Var n -> Var n
    | Constant v -> Constant v
    | Not exp -> Not (substitute p exp)
    | And exps -> And (Array.map (substitute p) exps)
    | Or exps -> Or (Array.map (substitute p) exps)
    | Join exps -> Join (Array.map (substitute p) exps)

let substitute_list p = List.fold_left (fun acc -> fun cur -> substitute acc cur) p

(**
  Get an expression in terms of only None (00) and High (01)
  i.e. the content is on the 'right'

  We can obtain this for a value x by computing x OR None
*)
let encode_right_weight = function
    | Non -> False
    | High -> True
    | Low -> False
    | Both -> True

let encode_left_heavy = function
    | Non -> (False, Join ([| Var 0 ; Constant False |]))
    | High -> (False, Not (Var 0))
    | Low -> (False, Var 0)
    | Both -> (True, Var 0)

let decode_right_weight ev = 
    let (translation, encoding) = ev in
    eval_belnap_logical_expression [| encoding |] translation

(* Printers *)

let rec logical_expression_to_string to_string = function
    | Var x -> "x" ^ string_of_int x
    | Constant v -> to_string v
    | Not exp -> 
        let exp = logical_expression_to_string to_string exp in
        "¬(" ^ exp ^ ")"
    | And exps -> 
        array_to_string exps "" "" " ∧ " (logical_expression_to_string to_string)
    | Or exps ->
        array_to_string exps "" "" " ∨\n\t" (logical_expression_to_string to_string)
    | Join exps ->
        array_to_string exps "" "" " ⊔ " (logical_expression_to_string to_string)

let belnap_expression_to_string = logical_expression_to_string belnap_value_to_string
let classical_expression_to_string = logical_expression_to_string classical_value_to_string