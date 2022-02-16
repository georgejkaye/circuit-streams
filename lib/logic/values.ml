open Core.Helpers

open Order

(* The classical set of values, True and False *)
type classical_value = 
    | True
    | False
    
let classical_value_to_string = function
    | True -> "t"
    | False -> "f"

(**
    Belnap's four value logic, Non, High, Low, and Both

    These values form two important lattices,
      - A4, the approximation lattice, which determines monotonicity
      - L4, the logical lattice, which determines and/or

                Both              High
               /    \            /    \
       A4    High   Low   L4   Non   Both
               \    /            \    /
                Non              Low

*)
type belnap_value =
    | Non
    | High
    | Low
    | Both

let belnap_value_to_string = function
| Non -> "N"
| High -> "T"
| Low -> "F"
| Both -> "B"

(**
  Belnap values can be expressed as pairs 
  of classical values
*)
let belnap_value_to_classical_string = function
| Non -> "00"
| High -> "01"
| Low -> "10"
| Both -> "11"

(**
   Less than on the lattice A4
*)
let lte x y = match (x, y) with
| (Non, Non) -> true
| (High, High) -> true
| (Low, Low) -> true
| (Non, _) -> false
| (_, Both) -> true
| (_, _) -> false

(* Computing input words *)

let all_inputs = [Non; High; Low; Both]

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
        (Non, [High;Low;Both]);
        (High, [Both]);
        (Low, [Both]);
        (Both, [])
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
    | Nor
    | Nand
    | AndN of int
    | OrN of int

let gate_to_string = function
| And -> "AND"
| Or  -> "OR"
| Not -> "NOT"
| Nor -> "NOR"
| Nand -> "NAND"
| Join -> "⊔"
| AndN n -> "AND_" ^ string_of_int n
| OrN n -> "OR_" ^ string_of_int n

let gate_inputs = function
| And -> 2
| Or -> 2
| Not -> 1
| Join -> 2
| Nor -> 2
| Nand -> 2
| AndN n -> n
| OrN n -> n

let and_b x y = match (x, y) with
| (High, x) -> x
| (x, High) -> x
| (Low, _) -> Low
| (_, Low) -> Low
| (Non, Non) -> Non
| (Non, Both) -> Low
| (Both, Non) -> Low
| (Both, Both) -> Both

let or_b x y = match (x, y) with
| (High, _) -> High
| (_, High) -> High
| (Low, x) -> x
| (x, Low) -> x
| (Non, Non) -> Non
| (Non, Both) -> High
| (Both, Non) -> High
| (Both, Both) -> Both

let not_b x = match x with
| Non -> Non
| High -> Low
| Low -> High
| Both -> Both

let join_b x y = match (x, y) with
| (Both, _) -> Both
| (_, Both) -> Both
| (Low, High) -> Both
| (High, Low) -> Both
| (Low, _) -> Low
| (_, Low) -> Low
| (High, _) -> High
| (_, High) -> High
| (Non, Non) -> Non

let eval_andn = List.fold_left and_b High
let eval_orn = List.fold_left or_b Low
let eval_joinn = List.fold_left join_b Non

let eval_gate g xs = match g with
| And -> and_b (List.nth xs 0) (List.nth xs 1)
| Or  -> or_b (List.nth xs 0) (List.nth xs 1)
| Not -> not_b (List.nth xs 0)
| Nor -> not_b (or_b (List.nth xs 0) (List.nth xs 1))
| Nand -> not_b (and_b (List.nth xs 0) (List.nth xs 1))
| Join -> join_b (List.nth xs 0) (List.nth xs 1)
| AndN _ -> eval_andn xs
| OrN _ -> eval_orn xs

(* Interpretation as classical logic *)

let belnap_to_classical = function
| Non -> (false, false)
| Low -> (true, false)
| High -> (false, true)
| Both -> (true, true)

let belnap_to_classical_list vs = 
    List.rev
        (List.fold_left 
            (fun acc -> fun (x,y) -> y :: x :: acc) 
            [] 
            (List.map belnap_to_classical vs)
        )

let list_of_inputs_to_input_list vss = 
    let length = get_max_length vss in
    List.init
        length
        (fun i ->
            List.rev (List.fold_left
                (fun acc -> fun cur -> 
                    let v = match (List.nth cur i) with
                        | v -> v
                        | exception (Failure _) -> Non
                in 
                    v :: acc
                )
            []
            vss
            )
        )

(* Printers *)

let value_list_to_string to_string vs = list_to_string vs "" "" "" to_string
let value_list_list_to_string to_string vss = list_to_string vss "[" "]" " ; " (value_list_to_string to_string)

let belnap_value_list_to_string = value_list_to_string belnap_value_to_string
let belnap_value_list_list_to_string = value_list_list_to_string belnap_value_to_string
let classical_value_list_to_string = value_list_to_string classical_value_to_string