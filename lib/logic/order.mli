(**
    Datatype for partial orders

    The elements are given as a list, and the ordering is given
    by associating each element with the elements that it is less than
    (the element itself does not need to be included in this order,
    the orderings are not strict)
*)
type 'a partial_order = { elements : 'a list; order : ('a * 'a list) list; }

(**
    Check if two elements are related by a partial order

    @param po The partial order
    @param x The first element
    @param y The second element
    @return If [x <= y]
*)
val po_lte : 'a partial_order -> 'a -> 'a -> bool

(**
    Compare two lists with a partial order pointwise

    @param po The partial order
    @param xs The first list
    @param ys The second list
    @return If [xs <= ys] pointwise
*)
val list_lte : 'a partial_order -> 'a list -> 'a list -> bool

(**
    Compare two arrays with a partial order pointwise

    @param po The partial order
    @param xs The first array
    @param ys The second array
    @return If [xs <= ys] pointwise
*)
val array_lte : 'a partial_order -> 'a array -> 'a array -> bool

(**
    Derive a new order from an existing one and a map to some new elements

    @param po The existing partial order
    @param map The map from elements of [po] to a new set
    @return The new partial order
*)
val derive_order_from_existing :
  'a partial_order -> ('a * 'b) list -> 'b partial_order

(**
    Derive a new order from two orders, where if an element is in 
    both orders, the less thans are propagated.

    @caution This could create degenerate partial orders

    @param po The first partial order
    @param qo The second partial order
    @return The new partial order
*)
val combine_orders : 'a partial_order -> 'a partial_order -> 'a partial_order

(**
    Print a partial order as a pretty string, in the form [x <= y, z \n y <= z]

    @param po The partial order
    @param print The print function
    @return The pretty string
*)
val partial_order_to_string : 'a partial_order -> ('a -> string) -> string
