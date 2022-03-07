let nats n = List.init n (fun x -> x)
let nats_from init n = List.init (n-1) (fun x -> x + init)

let rec revnats n = match n with
| 0 -> [0]
| n -> n :: revnats (n-1)

let array_nats n = Array.init n (fun x -> x)

let nats_of xs = nats (Array.length xs)
