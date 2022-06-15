open Logic.Values
open Helpers.Nats
open Expression

type 'a truth_table = {
  inputs: int;
  outputs: int;
  rows: int;
  (** Each row in the array contains an array of inputs and an array of outputs *)
  matrix: ('a array * 'a array) array;
}

(**
    Convert a classical truth table into a series of dnfs, one for each
    column of the output. Each clause of the dnf is defined as the conjunct 
    of all the truth values in a row of the truth table, negated if the output 
    is false.
*)
let convert_classical_table_to_dnf tt = 
    let convert_classical_table_to_dnf_column i =
        let convert_row_to_cnf row = 
            let (inputs, outputs) = row in
            let conjuncts = And (
                Array.map 
                    (fun j ->  if inputs.(j) == True then Var j else Not (Var j)) 
                    (array_nats tt.inputs)
                ) 
            in
            if outputs.(i) == True then
                conjuncts
            else
                Not conjuncts
        in
        Or(
            Array.of_list 
                (List.rev 
                    (List.fold_left
                        (fun acc -> 
                            fun j -> 
                                if (snd (tt.matrix.(j))).(i) == True then convert_row_to_cnf (tt.matrix.(j)) :: acc else acc
                        )
                        [] 
                        (nats tt.rows)
                    )
                )
        )
    in 
    List.map convert_classical_table_to_dnf_column (nats tt.outputs)

(* Printer *)

let truth_table_to_string to_string tt =
    let rows = 
        List.map 
            (fun i -> 
                let (inputs, outputs) = tt.matrix.(i) in 
                to_string (Array.to_list inputs) ^ " | " ^ to_string (Array.to_list outputs)
            )
        (nats tt.rows)
  in
  List.fold_left
      (fun acc -> fun cur -> acc ^ "\n" ^ cur)
      (List.hd rows)
      (List.tl rows)


let convert_truth_table tt translate =
    let matrix = 
        Array.map 
            (fun (inputs, outputs) ->
                (
                    Array.map translate inputs, 
                    Array.map translate outputs
                )
            )
            tt.matrix
    in
    { 
        inputs = tt.inputs;
        outputs = tt.outputs;
        rows = tt.rows;
        matrix = matrix
    }

let belnap_truth_table_to_positive_classical_table tt =
    let translate_to_positive = function
        | Non  -> False
        | Low  -> False
        | High -> True
        | Both -> True
    in
    convert_truth_table tt translate_to_positive

let belnap_truth_table_to_negative_classical_table tt =
        let translate_to_negative = function
            | Non  -> False
            | Low  -> True
            | High -> False
            | Both -> True
        in
        convert_truth_table tt translate_to_negative

let positive_table_to_belnap tt =
    let translate_to_belnap = function
        | False -> Non
        | True -> High
    in
    convert_truth_table tt translate_to_belnap

let negative_table_to_belnap tt =
        let translate_to_belnap = function
            | False -> Non
            | True -> Low
        in
        convert_truth_table tt translate_to_belnap

(* Printers *)

let belnap_truth_table_to_string = truth_table_to_string belnap_value_list_to_string
let classical_truth_table_to_string = truth_table_to_string classical_value_list_to_string