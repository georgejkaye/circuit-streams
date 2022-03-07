(**
    The classical logic values
*)
type classical_value = True | False

(**
    Print True as t and False as f
*)
val classical_value_to_string : classical_value -> string

(**
    Belnap's four value logic, Non(e), High, Low, and Both
    None corresponds to no signal, High and Low each correspond
    to receiving one signal, and Both corresponds to receiving 
    both at the same time (a short circuit)

    These values form two important lattices:
      - A4, the approximation lattice, which determines monotonicity
      - L4, the logical lattice, which determines the logical operations

*)
type belnap_value = Non | High | Low | Both

(**
    Print None as N, High as T, Low as F, Both as B
*)
val belnap_value_to_string : belnap_value -> string

(**
    Belnap values can be expressed as pairs of classical values:
    None as 00, High as 10, Low as 01, Both as 11
*)
val belnap_value_to_classical_string : belnap_value -> string

(**
    Less than on the lattice A4, i.e. N < {T, F} < B
*)
val lte : belnap_value -> belnap_value -> bool

(**
    Get a list of all Belnap inputs, i.e. [N;T;F;B]
*)
val all_inputs : belnap_value list

(**
    Get all inputs of a given width in bits
    TODO this is quite slow for n > 4
*)
val all_inputs_of_width : int -> belnap_value array list

(**
    The partial order A4 on the Belnap values, i.e. N < {T,F} < B
*)
val value_order : belnap_value Order.partial_order

(**
    The ordering A4 applied pointwise to strings of Belnap values
*)
val value_string_order : int -> belnap_value array Order.partial_order

(**
    Translate a list of sequences of inputs to an array of Belnap strings, 
    where the ith value is the concatenation of the ith element of each 
    of the input lists
*)
val transpose_inputs :
  belnap_value list list -> belnap_value array array

(**
    Construct a waveform from segments of repeated values.
    For example, [make_waveform [(Low, 2); (High, 2)]]
    produces [Low; Low; High; High]

    @param xs The list of pairs [(v,i)] to construct a waveform from: [(v,i)] means
    pulse value [v] for [i] ticks.
*)
val make_waveform : ('a * int) list -> 'a list

(**
    Construct a one-bit clock waveform that alternates between High
    and Low.

    @param p The period of the clock, i.e. how long it remains
    at each value
    @param n The length of this waveform
    @param v The value this clock begins on
    @return The clock waveform
*)
val make_clock : int -> int -> bool -> belnap_value list

val value_list_to_string : ('a -> string) -> 'a list -> string
val value_array_to_string : ('a -> string) -> 'a array -> string
val value_list_list_to_string : ('a -> string) -> 'a list list -> string
val value_array_array_to_string : ('a -> string) -> 'a array array -> string
val belnap_value_list_to_string : belnap_value list -> string
val belnap_value_array_to_string : belnap_value array -> string
val belnap_value_list_list_to_string : belnap_value list list -> string
val belnap_value_array_array_to_string : belnap_value array array -> string
val classical_value_list_to_string : classical_value list -> string
