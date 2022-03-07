open Helpers.Lists
open Helpers.Arrays

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
    in let all = remove_duplicates (all_inputs_of_length' n)
    in List.map Array.of_list all

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
            let less_thans = List.filter (array_lte value_order vs) all_values
            in (vs, less_thans))
        all_values
    }
    
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

let list_of_inputs_to_input_array vss = 
    let length = get_max_length vss in
    Array.init
        length
        (fun i ->
            Array.of_list 
                (List.rev 
                    (List.fold_left
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
        )

(**
    Given a list of pairs (value v, int i), create a waveform that
    produces each value v in order for i ticks.
*)
let make_waveform xs = 
    List.fold_left
        (fun acc -> fun (v, n) ->
            let values = List.init n (fun _ -> v) in
            acc @ values 
        )
        []
        xs 
    
(**
    Create a clock waveform that alternates high and low for n ticks at a time,
    for k iterations
*)
let make_clock n k start =
    List.init (n*(k*2)) (fun i -> 
        if i mod (2 * n) < n then if start then High else Low else if start then Low else High)


(* Printers *)

let value_list_to_string to_string vs = list_to_string vs "" "" "" to_string
let value_array_to_string to_string vs = array_to_string vs "" "" "" to_string
let value_list_list_to_string to_string vss = list_to_string vss "[" "]" " ; " (value_list_to_string to_string)
let value_array_array_to_string to_string vss = array_to_string vss "[" "]" " ; " (value_array_to_string to_string)

let belnap_value_list_to_string = value_list_to_string belnap_value_to_string
let belnap_value_array_to_string = value_array_to_string belnap_value_to_string
let belnap_value_list_list_to_string = value_list_list_to_string belnap_value_to_string
let belnap_value_array_array_to_string = value_array_array_to_string belnap_value_to_string
let classical_value_list_to_string = value_list_to_string classical_value_to_string