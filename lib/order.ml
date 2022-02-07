open Helpers

type 'a partial_order = {
    elements: 'a list;
    order: ('a * 'a list) list
}

let po_lte po x y = if x == y then true else match List.assoc_opt x po.order with
    | None -> false
    | Some ys -> List.mem y ys

let list_lte po = List.fold_left2 (fun acc -> fun x -> fun y -> acc && po_lte po x y) true
        

let derive_order_from_existing po map =
    let new_order = List.map (fun (p,ps) -> (List.assoc p map, List.map (fun p -> List.assoc p map) ps)) po.order in
    {
        elements = List.map snd map;
        order = new_order;
    }

let combine_orders po qo =
    let new_empty_order = List.fold_left
        (fun (elements, ordering) -> fun cur -> (cur :: elements, (cur, []) :: ordering)) 
        ([],[]) 
        po.elements 
    in
    let new_empty_order = List.fold_left 
        (fun (elements, ordering) -> fun cur -> 
            if List.mem cur po.elements 
            then (elements, ordering) 
            else (cur :: elements, (cur, []) :: ordering))
        new_empty_order
        qo.elements
    in
    let new_ordering = List.map (fun (p, _) -> 
        let p_elements = match List.assoc_opt p po.order with
            | None -> []
            | Some xs -> xs
        in
        let q_elements = match List.assoc_opt p qo.order with 
            | None -> []
            | Some xs -> xs 
        in 
        let pq_elements = List.fold_left 
            (fun acc -> fun cur -> match List.assoc_opt cur qo.order with
                | None -> acc
                | Some xs -> acc @ xs
            ) [] p_elements
        in let qp_elements = List.fold_left
            (fun acc -> fun cur -> match List.assoc_opt cur po.order with
                | None -> acc
                | Some xs -> acc @ xs
            ) [] q_elements
        in
        (p, p_elements @ q_elements @ pq_elements @ qp_elements)
        
    ) (snd new_empty_order)
    in
    {
        elements = fst new_empty_order;
        order = new_ordering
    }

(* Printers *)

let partial_order_to_string po print = 
    let partial_order_entry_to_string poe = 
        let (p, ps) = poe in 
        let ps = remove p (remove_duplicates ps) in
        let element = (print p) ^ " <= " in
        let lessthans = list_to_string_def ps print in
        element ^ lessthans
    in
    list_to_string po.order "" "" "\n" partial_order_entry_to_string
