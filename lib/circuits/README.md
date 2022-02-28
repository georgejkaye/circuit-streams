
# `circuits` library

The key sequence of axioms used to evaluate circuits is the *streaming* axiom followed by an instance of *gate* to evaluate the 'now' copy.
When using the syntactic sugar of depicting 'initial values' inside the delay bubbles, it is clear to see that this corresponds to propagating the values through the gate.

![Application of axioms to evaluate streaming](../../docs/eval.svg)

The inputs to a gate are evaluated, and then this propagation is performed.
Because of this, if there is a 'true' non-delay-guarded feedback loop, a stack overflow will occur.
We can still reason with these circuits by applying the *instant feedback* axiom to replace the loop with a series of copies of the circuit, but this must be done manually.
