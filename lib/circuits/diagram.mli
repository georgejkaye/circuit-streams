(**
    Generate dot code for a diagram corresponding to a circuit

    @param c The circuit
    @return The dot code for this circuit
*)
val dot_of_circuit : Core.circuit -> string

(**
    Write the dot code for a circuit to a file

    @param c The circuit
    @param f The name of the file
*)
val write_circuit_to_file : Core.circuit -> string -> unit
