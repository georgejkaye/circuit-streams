open Belnap
open Helpers

type truth_table = {
  inputs: int;
  outputs: int;
  table: (value list * value list) list
}

let right_weight_truth_table tt = 
    let (translations, translated) = (List.fold_left
        (fun (translations, translated) -> fun ((ms,ns)) ->
            let (row_translation, row_translated) =
                let translate_list xs =  
                    let (list_l, list_r) =
                        List.fold_left
                        (fun (acc_l,acc_r) -> fun cur -> 
                            let (translation, value) = encode_right_weight cur in
                            (translation :: acc_l, value :: acc_r)
                        )
                        ([],[])
                        xs
                    in
                    (List.rev list_l, List.rev list_r)
                in
                let (input_translation, input_translated) = translate_list ms in
                let (output_translation, output_translated) = translate_list ns in
                ((input_translation, output_translation), (input_translated, output_translated))
            in
            row_translation :: translations, row_translated :: translated
        )
        ([],[])
        tt.table
    )
  in
  ({
    inputs= tt.inputs;
    outputs= tt.outputs;
    table=translated
  }, translations)

let convert_classical_table_to_dnf tt = 
    let convert_classical_table_to_dnf_cell i =
        let convert_row_to_cnf row = 
            let (inputs, outputs) = row in
            let conjunct = Gate (AndN (List.length inputs), (List.map (fun x -> Constant x) inputs)) in
            if List.nth outputs i == True then
                conjunct
            else
                Gate(Not, [conjunct])
        in
        let disjuncts = List.map convert_row_to_cnf tt.table
        in
        Gate (OrN (List.length disjuncts), disjuncts)
    in 
    List.map convert_classical_table_to_dnf_cell (nats tt.outputs)
(* Printer *)

let truth_table_to_string belnap tt =
  let rows = 
      List.map 
      (fun (vs, ws) -> 
          value_list_to_string belnap vs ^ " | " ^ value_list_to_string belnap ws
      )
      tt.table
  in
  List.fold_left
      (fun acc -> fun cur -> acc ^ "\n" ^ cur)
      (List.hd rows)
      (List.tl rows)


