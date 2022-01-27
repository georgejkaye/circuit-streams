open Belnap
open Streams

type mealy = {
    inputs: int;
    outputs: int;
    states: int;
    transition_function: ((int * value list) * int) list;
    output_function: ((int * value list) list) list
}


let stream_to_mealy cs = failwith "todo"