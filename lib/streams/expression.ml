open Logic.Values
open Logic.Gates 
open Helpers.Arrays

(* Weighting on one side of the classical interpretation *)

type 'a logical_expression =
    | Var of int
    | Constant of 'a
    | Not of 'a logical_expression
    | And of 'a logical_expression array
    | Or of 'a logical_expression array
    | Join of 'a logical_expression array

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

let rec convert_logical_expression translate = function
    | Var n -> Var n
    | Constant v -> Constant (translate v)
    | Not exp -> Not (convert_logical_expression translate exp)
    | And exps -> And (Array.map (convert_logical_expression translate) exps)
    | Or exps -> Or (Array.map (convert_logical_expression translate) exps)
    | Join exps -> Join (Array.map (convert_logical_expression translate) exps)

let positive_expression_to_belnap =
    convert_logical_expression
        (function
            | False -> Non
            | True -> High
        )
let negative_expression_to_belnap =
    convert_logical_expression
        (function
            | False -> Non
            | True -> Low
        )

let rec eval_belnap_logical_expression xs exp = 
    match exp with
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

let rec eval_positive_logical_expression xs exp = 
    match exp with
    | Var n -> xs.(n)
    | Constant v -> v
    | Not exp -> 
        let exp = eval_positive_logical_expression xs exp in
        classical_not exp
    | And exps -> 
        let exps = Array.map (eval_positive_logical_expression xs) exps in
        classical_andn exps
    | Or exps ->
        let exps = Array.map (eval_positive_logical_expression xs) exps in
        classical_orn exps
    | Join _ ->
        failwith "no join please"

let rec eval_negative_logical_expression xs exp = 
    match exp with
    | Var n -> xs.(n)
    | Constant v -> v
    | Not exp -> 
        let exp = eval_negative_logical_expression xs exp in
        classical_not exp
    | And exps -> 
        let exps = Array.map (eval_negative_logical_expression xs) exps in
        classical_orn exps
    | Or exps ->
        let exps = Array.map (eval_negative_logical_expression xs) exps in
        classical_andn exps
    | Join _ ->
        failwith "no join please"

let rec substitute p = function
    | Var 0 -> p
    | Var n -> Var n
    | Constant v -> Constant v
    | Not exp -> Not (substitute p exp)
    | And exps -> And (Array.map (substitute p) exps)
    | Or exps -> Or (Array.map (substitute p) exps)
    | Join exps -> Join (Array.map (substitute p) exps)

let substitute_list p = List.fold_left (fun acc -> fun cur -> substitute acc cur) p