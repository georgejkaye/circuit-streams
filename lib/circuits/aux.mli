val get_output_names : Core.circuit -> string array
val get_output_ports : Core.circuit -> Core.port array
val gates : Core.circuit -> int

(**
    Generic traversal function with an accumulator.

    @param f A function a -> port -> a that takes an accumulator
    and a port, and can mutate the port or update the accumulator
    @param acc The initial value of the accumulator
    @param c The circuit
    @return The final accumulator
*)
val traverse : ('a -> Core.port -> 'a) -> 'a -> Core.circuit -> 'a
val get_inputs : Core.circuit -> int
val get_outputs : Core.circuit -> int
val increment_inputs : int -> Core.circuit -> unit
val get_output_port : Core.circuit -> int -> Core.port
