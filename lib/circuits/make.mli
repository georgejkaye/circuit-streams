(**
    Combine multiple subcircuits into one circuit

    @param circuits An array where each element (c, ps) is a tuple
    containing a circuit c and an array of ports ps to connect to each
    of its inputs
    @param outputs An array determining the outputs of the new
    circuit. Each element outputs.(i) is a tuple (j,k,d,s) describing that 
    the ith output of the new circuit will be the kth output of 
    circuits.(j), delayed by d ticks, and labelled with label s
    @param input_names The names for the inputs of the new circuit

    @return The new combined circuit
*)
val combine :
  (Core.circuit * Core.port array) array ->
  (int * int * int * string) array -> string array -> Core.circuit

(**
    Iterate a circuit multiple times to simulate the 
    stabilisation of 'instant feedback'

    @param id The starting id of the blocks
    @param c The circuit building function (int -> int * circuit)
    that takes an id and returns the new id and the circuit
    @param x The number of outputs to feedback
    @param m The number of inputs of the circuit
    @param n The number of outputs of the circuit
    @param inputs The names of the inputs of the new circuit
    @param outputs The names of the outputs of the new circuit

    @return The circuit with the instant feedback axiom applied
*)
val iterate :
  'a ->
  ('a -> 'a * Core.circuit) ->
  int -> int -> int -> string array -> string array -> 'a * Core.circuit

(**
    Put multiple circuits in parallel so their outputs can
    be compared. Each circuit is assigned an identifier string
    so that the outputs of circuits with the same name can be 
    distinguished (e.g. different implementations of the same circuit)

    @param cs A list of pairs of an identifier string and a circuit
    @return The circuits all put in parallel.
*)
val compare : (string * Core.circuit) list -> Core.circuit
