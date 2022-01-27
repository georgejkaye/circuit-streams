open Belnap
open Streams
open Helpers

(* Example *)

let f0 i = 
    [
        Value True ;
        Value Bot
    ]

let f1 i = 
    [
        Value Bot ;
        Arg (i - 1, 0)
    ]

let f2 i = 
    [
        Value Bot ;
        Value False
    ]

let f : circuit_stream = (1, 1, [f0], [f1;f2])

let () = 
    let sigma = [Top] :: (constant_stream [True] 10) in
    let evaled = eval f 6 sigma in
    print_value_list_list evaled