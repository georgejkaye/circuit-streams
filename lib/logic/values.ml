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
| High -> "10"
| Low -> "01"
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

let all_inputs = [Non; High; Low; Both]

let all_inputs_of_width n = 
    let rec all_inputs_of_width' = function
        | 0 -> [[]]
        | n -> let rest = all_inputs_of_width' (n-1) in
            List.fold_left (fun acc -> fun v -> 
                    acc @ List.fold_left (fun acc -> fun cur -> acc @ (all_insertions v cur)) [] rest 
            )
            []
            all_inputs
    in let all = remove_duplicates (all_inputs_of_width' n)
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

let value_string_order n = 
    let all_values = all_inputs_of_width n in
    {
        elements = all_values;
        order = List.map 
        (fun vs -> 
            let less_thans = List.filter (array_lte value_order vs) all_values
            in (vs, less_thans))
        all_values
    }

let transpose_inputs vss = 
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

let make_waveform xs = 
    List.fold_left
        (fun acc -> fun (v, n) ->
            let values = List.init n (fun _ -> v) in
            acc @ values 
        )
        []
        xs 
    
let make_clock p n v =
    List.init (p*(n*2)) (fun i -> 
        if i mod (2 * p) < p then if v then High else Low else if v then Low else High)

let value_list_to_string to_string vs = list_to_string vs "" "" "" to_string
let value_array_to_string to_string vs = array_to_string vs "" "" "" to_string
let value_list_list_to_string to_string vss = list_to_string vss "[" "]" " ; " (value_list_to_string to_string)
let value_array_array_to_string to_string vss = array_to_string vss "[" "]" " ; " (value_array_to_string to_string)

let belnap_value_list_to_string = value_list_to_string belnap_value_to_string
let belnap_value_array_to_string = value_array_to_string belnap_value_to_string
let belnap_value_list_list_to_string = value_list_list_to_string belnap_value_to_string
let belnap_value_array_array_to_string = value_array_array_to_string belnap_value_to_string
let classical_value_list_to_string = value_list_to_string classical_value_to_string