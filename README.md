# circuit-streams

This repo contains some implementations of work I have been doing on a *categorical semantics for digital circuits*, based on previous work by Ghica and Jung.

## Evaluating circuits

The categorical semantics provides numerous advantages, including an intuitive way of modelling circuits with delays in the wires (which will always be present in 'real' situations) and on a more idealised side, for dealing with `instantaneous feedback' where there is no such delay.

The `circuits` library provides ways to specify circuits with varying amounts of delays, and then evaluate them.
This is done by using a slightly simplified version of the axiomatic semantics: inputs are propagated throughout the circuit and transformed into outputs.
Crucially, the `instant feedback' axiom is not applied automatically, so not all circuits defined in this way are productive (they will instead cause a stack overflow).
This axiom is potentially difficult to implement automatically with this model, but a function is provided that can be used manually to transform circuits with designated feedback inputs and outputs into an iterated copy.

## Circuits as streams

Circuits are in one-to-one correspondence with a special class of *stream functions*.
At a high level, the class of stream functions we are interested in has three properties:

* They are **causal**: the *n*th element of the output depends only on the first *n+1* inputs;
* They are **monotone**: the initial output is a monotone function, and the stream derivative produces another monotone stream function; and
* They have **finite stream derivatives**: from a given stream function, there always exists at least one finite word *w* such that the stream derivatives obtained by inputting *w* contains at least one duplicate.

## Background

This program is based on work on my paper with Dan Ghica and David Sprunger, '[Full abstraction for digital circuits](https://arxiv.org/abs/2201.10456)'.