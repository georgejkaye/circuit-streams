let create_full_string xs = 
    List.fold_left (
        fun acc -> fun cur -> acc ^ "\n\t" ^ cur)
        ("\t" ^ List.hd xs)
        (List.tl xs)

let create_graph_options_string xs =
    "\t" ^ 
    List.fold_left 
        (fun acc -> fun (property, value) -> acc ^ "\n\t" ^ property ^ "=" ^ value)
        (fst (List.hd xs) ^ "=" ^ snd (List.hd xs))
        (List.tl xs)

let create_options_string selector xs = 
    "\t" ^ List.fold_left (
        fun acc -> fun (property, value) -> acc ^ ", " ^ property ^ "=" ^ value
    )
    (selector ^ "[" ^ (fst (List.hd xs)) ^ "=" ^ (snd (List.hd xs)))
    (List.tl xs) ^ "]"

let write_to_file f s = 
    let oc = open_out f in
        Printf.fprintf oc "%s" s;
        close_out oc