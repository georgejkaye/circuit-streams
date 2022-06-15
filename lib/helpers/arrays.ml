open Nats

let array_to_string xs lbracket rbracket delimiter to_string = 
    let len = Array.length xs in
    let content = 
        if len == 0 
            then 
                "" 
            else
                let first = to_string xs.(0)
                in if len == 1 
                    then 
                        first 
                    else 
                        List.fold_left 
                            (fun acc -> fun i -> acc ^ delimiter ^ to_string xs.(i)) 
                            first 
                            (nats_from 1 len)
    in
    lbracket ^ content ^ rbracket

let array_to_string_def xs to_string = array_to_string xs "[" "]" ";" to_string