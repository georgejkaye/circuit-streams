open Logic.Values
open Core.Helpers
open Expression

type 'a truth_table = {
  inputs: int;
  outputs: int;
  rows: int;
  (** Each row in the array contains an array of inputs and an array of outputs *)
  matrix: ('a array * 'a array) array;
}

type 'a translation_table = {
    translations: ('a logical_expression array * 'a logical_expression array) array;
}

(**
   'Right weight' a belnap truth table, that is, translate it into a form
    using only True and Bot (which correspond to classical tuples 01 and 00 
    respectively, hence 'right weighting'). We can then convert each belnap
    value into a classical value based on its right element.

    Returns a classical truth table, and a list of `translations`: a list 
    of logical expressions to translate each row of the classical table back 
    into the original belnap value later.
*)
let right_weight_truth_table tt = 
    let weighteds = 
        List.fold_left 
            (fun weighteds -> fun i ->
                let row = tt.matrix.(i) in
                let translate_array arr n =
                    let weighted = 
                        List.fold_left
                            (fun weighted -> fun i ->
                                let cur = arr.(i) in
                                let current_weighted = encode_right_weight cur in
                                current_weighted :: weighted
                            )
                            []
                            (nats n)
                    in
                    Array.of_list (List.rev weighted)
                in
                let input_weighted = translate_array (fst row) tt.inputs in
                let output_weighted = translate_array (snd row) tt.outputs in
                (input_weighted, output_weighted) :: weighteds
            )
            []
            (nats tt.rows)
    in
    {
        inputs = tt.inputs;
        outputs = tt.outputs;
        rows = tt.rows;
        matrix = Array.of_list (List.rev weighteds)
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
                List.map 
                    (fun j ->  if inputs.(j) == True then Var j else Not (Var j)) 
                    (nats tt.inputs)
                ) 
            in
            if outputs.(i) == True then
                conjuncts
            else
                Not conjuncts
        in
        Or(
            List.rev (List.fold_left
                (fun acc -> fun j ->
                    if (snd (tt.matrix.(j))).(i) == True then convert_row_to_cnf (tt.matrix.(j)) :: acc else acc)
                [] 
                (nats tt.rows)
            ))
    in 
    List.map convert_classical_table_to_dnf_column (nats tt.outputs)

let decode_right_weighted_dnf translations row dnf = 
    let translation_matrix = translations.translations in
    let decoded = match dnf with    
        | Or xs ->
            let decoded_term = List.mapi 
                (fun i -> fun clause ->
                    let decoded_clause =
                        let decode_clause =
                            List.mapi
                            (fun _ -> function
                                | Var n -> substitute (Var n) (fst translation_matrix.(i)).(n)
                                | Not (Var n) -> Not (substitute (Var n) (fst translation_matrix.(i)).(n))
                                | _ -> failwith "[decode_right_weighted_dnf] Element in clause in dnf is not an atom"
                            )
                        in
                        match clause with
                        | And xs -> And (decode_clause xs)
                        | Not (And xs) -> Not (And (decode_clause xs))
                        | _ -> failwith "[decode_right_weighted_dnf] Clause in dnf is not a conjunction"
                    in
                    substitute decoded_clause (snd translation_matrix.(i)).(row)
                )
                xs
            in
            Or decoded_term
        | _ -> failwith "[decode_right_weighted_dnf] Expression is not in dnf"
    in
    decoded

let decode_right_weighted_dnfs dnfs translation = 
    List.mapi (decode_right_weighted_dnf translation) dnfs

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

(* Printers *)

let belnap_truth_table_to_string = truth_table_to_string belnap_value_list_to_string
let classical_truth_table_to_string = truth_table_to_string classical_value_list_to_string