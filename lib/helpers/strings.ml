let get_longest_string xs = Array.fold_left (fun acc -> fun cur -> max acc (String.length cur)) 0 xs

let pad_back n s = 
    let length = String.length s in
    if n < length 
        then 
            s 
        else
            let i = n - length in
            let ws = String.init i (fun _ -> ' ') in
            s ^ ws