
(**
    Simulate a circuit for a given number of ticks and inputs

    @param n The number of ticks to simulate for
    @param inputs The inputs to provide to the circuit, inputs.(i).(j) is the
    jth element of the input at tick i
    @param c The circuit to simulate

    @return The array of outputs, outputs.(i).(j) is the jth 
    element of the output at tick i
*)
val simulate_circuit : int -> Logic.Values.belnap_value array array ->
    Core.circuit -> Logic.Values.belnap_value array array

(**
    Get a string representing the outputs of a circuit over time,
    represented as a 'truth table'.
    
    @param n The number of ticks to simulate for
    @param inputs The inputs to provide to the circuit, inputs.(i).(j) is the
    jth element of the input at tick i
    @param c The circuit to simulate
    
    @return A string containing a the 
*)
val string_of_simulation :
    int -> Logic.Values.belnap_value array array -> Core.circuit -> unit
