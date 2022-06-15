open Values

type gate = 
    | And
    | Or
    | Not
    | Join
    | Nor
    | Nand
    | AndN of int
    | OrN of int
    | NandN of int
    | NorN of int

let gate_to_string = function
| And -> "AND"
| Or  -> "OR"
| Not -> "NOT"
| Nor -> "NOR"
| Nand -> "NAND"
| Join -> "⊔"
| AndN n -> "AND_" ^ string_of_int n
| OrN n -> "OR_" ^ string_of_int n
| NandN n -> "NAND_" ^ string_of_int n
| NorN n -> "NOR_" ^ string_of_int n

let gate_inputs = function
| And -> 2
| Or -> 2
| Not -> 1
| Join -> 2
| Nor -> 2
| Nand -> 2
| AndN n -> n
| OrN n -> n
| NandN n -> n
| NorN n -> n

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

let andn_b = Array.fold_left and_b High
let orn_b = Array.fold_left or_b Low
let eval_joinn = Array.fold_left join_b Non

let nandn_b xs = not_b (andn_b xs)
let norn_b xs = not_b (orn_b xs)


let eval_gate g xs = match g with
| And -> and_b xs.(0) xs.(1)
| Or  -> or_b xs.(0) xs.(1)
| Not -> not_b xs.(0)
| Nor -> not_b (or_b xs.(0) xs.(1))
| Nand -> not_b (and_b xs.(0) xs.(1))
| Join -> join_b xs.(0) xs.(1)
| AndN _ -> andn_b xs
| OrN _ -> orn_b xs
| NandN _ -> nandn_b xs
| NorN _ -> norn_b xs

let classical_not = function
| True -> False
| False -> True

let classical_and = function
| (False, _) -> False
| (True, x) -> x

let classical_or = function
| (True, _) -> True
| (False, x) -> x

let classical_andn = Array.fold_left (fun acc -> fun cur -> classical_and (acc, cur)) True
let classical_orn = Array.fold_left (fun acc -> fun cur -> classical_or (acc, cur)) False